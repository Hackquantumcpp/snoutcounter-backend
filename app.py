from dash import Dash, html, callback, dcc
from dash.dependencies import Input, Output
import dash_bootstrap_components as dbc
from datawrapper import Datawrapper

from keys import API_KEY, APPROVAL_CHART_KEY, APPROVAL_TABLE_KEY

# dw = Datawrapper(API_KEY)

app = Dash(__name__, external_stylesheets=[dbc.themes.ZEPHYR, dbc.icons.FONT_AWESOME])

color_mode_switch =  html.Span(
    [
        dbc.Label(className="fa fa-moon", html_for="switch"),
        dbc.Switch( id="switch", value=True, className="d-inline-block ms-1", persistence=True),
        dbc.Label(className="fa fa-sun", html_for="switch"),
    ]
)

# app.index_string += '\n<script src="https://cdn.jsdelivr.net/gh/ncase/nutshell/nutshell.js"></script>\n'
# app.index_string = """
# <!DOCTYPE html>
# <html>
# <head>
#     <title>SnoutCounter</title>

#     <!-- Keep Dash's Styles and Bootstrap -->
#     {%metas%}
#     <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css">
#     {%favicon%}
#     {%css%}
    
#     <!-- Nutshell Script -->
#     <script src="https://cdn.jsdelivr.net/gh/ncase/nutshell/nutshell.js"></script>
#     <script>
#         function reinitializeNutshell() {
#             setTimeout(() => { 
#                 if (typeof nutshell !== 'undefined') {
#                     nutshell.init();
#                 } 
#             }, 500);  // Delay to detect dynamic updates
#         }

#         document.addEventListener("DOMContentLoaded", reinitializeNutshell);

#         // Observe changes in the page for dynamic updates
#         const observer = new MutationObserver(reinitializeNutshell);
#         observer.observe(document.body, { childList: true, subtree: true });
#     </script>

# </head>
# <body>
#     <div id="react-entry-point">
#         {%app_entry%}
#     </div>
#     <footer>
#         {%config%}
#         {%scripts%}
#         {%renderer%}
#     </footer>
# </body>
# </html>
# """

embed_code_approval = f'<iframe src="https://datawrapper.dwcdn.net/{APPROVAL_CHART_KEY}/" frameborder="0" style="width:100%;height:400px;"></iframe>'
approval_chart_version = 2

# nutshell = "https://cdn.jsdelivr.net/gh/ncase/nutshell/nutshell.js"
# app.scripts.append_script({"external_url": nutshell})

app.title = "SnoutCounter"


navbar = dbc.Navbar(
    dbc.Container([
        html.A(
            dbc.Row([
                dbc.Col(html.Img(height='75px', id='logo')),
            ], align='center', className='g-0'),
            href='/', style={'textDecoration': 'none'}
        ),
        dbc.Nav([
            dbc.NavItem(dbc.NavLink("Home", href="/")),
            dbc.NavItem(dbc.NavLink("About", href="/")),
            color_mode_switch
        ], className="ms-auto", navbar=True)
    ], fluid=True), id='navbar', sticky=True,
    # color="light", dark=False
)


app.layout = html.Div(children=[
    html.Div([
        html.Script(src="https://cdn.jsdelivr.net/gh/ncase/nutshell/nutshell.js"),
        html.Script(children="""
        document.addEventListener("DOMContentLoaded", function() {
            nutshell.init();
        });
        """)
    ]),
    navbar,
    # dbc.NavbarSimple(
    #     dbc.Container(
    #         [
    #             html.A(
    #                 dbc.Row(children=[
    #                     dbc.NavbarBrand(
    #                     html.Img(
    #                         src='/assets/snoutcounter.png',
    #                         height="75px",
    #                         width="162px"
    #                     ), href="/"
    #                 )
    #                 ], align='center')
    #             ),
    #             dbc.Nav(
    #                 [
    #                     dbc.NavItem(dbc.NavLink("Home", href="/")),
    #                     dbc.NavItem(dbc.NavLink("About", href="/")),
    #                 ], navbar=True
    #             )
    #         ]
    #     )
    # ),
    # html.Div(children=[html.Img(
    #         src="/assets/snoutcounter.png",
    #         style={
    #             "display": "block",
    #             "marginRight": "auto",
    #             "width":"162px",
    #             "height":"75px",
    #         }
    #     )], style={
    #         "justify-content": "left",
    #     }
    # ),
    html.Hr(),
    html.B(html.H1("At A Glance", style={'textAlign':'center', 'marginTop':'20px', 'marginBottom':'20px'})),
    html.Hr(),
    html.P(children="This is the landing page for SnoutCounter's polling averages.", style={'textAlign':'center'}),
    html.P(html.A(":Methodology", href='https://ncase.me/faq/#GoodMentalHealth'), style={'textAlign':'center'}),
    html.Script(children="""
    function reinitializeNutshell() {
        setTimeout(() => { 
            if (typeof nutshell !== 'undefined') {
                nutshell.init();
            } 
        }, 500);  // Give it time to detect new content
    }

    document.addEventListener("DOMContentLoaded", reinitializeNutshell);

    // Observe changes to the body (handles Dash updates)
    const observer = new MutationObserver(reinitializeNutshell);
    observer.observe(document.body, { childList: true, subtree: true });
    """),
    html.Div([
        html.Div([
            html.Iframe(
                # src=f"https://datawrapper.dwcdn.net/{APPROVAL_CHART_KEY}/",
                style={
                    "width": "75%",
                    "height": "600px",
                    "border": "0",
                }, id="approval-chart"
            )
        ], style = {
            "justify-content": "center",
            "align-items": "center",
            "display": "flex",
        })
    ]),
    html.Div(children=[
        html.Iframe(
            # src=f"https://datawrapper.dwcdn.net/{APPROVAL_TABLE_KEY}/",
            style={
                "width": "75%",
                "height": "700px",
                "border": "0",
            }, id="approval-table"
        )
    ], style = {
        "justify-content": "center",
        "align-items": "center",
        "display": "flex",
    }),
])

app.clientside_callback(
    """
    (switchOn) => {
       document.documentElement.setAttribute("data-bs-theme", switchOn ? "light" : "dark"); 
       return window.dash_clientside.no_update
    }
    """,
    Output("switch", "id"),
    Input("switch", "value"),
)

@callback(
    Output('approval-chart', 'src'),
    Input('switch', 'value')
)
def update_chart_color_mode(color_mode):
    if color_mode:
        return f"https://datawrapper.dwcdn.net/{APPROVAL_CHART_KEY}/"
    else:
        return f"https://datawrapper.dwcdn.net/{APPROVAL_CHART_KEY}/?dark=true"

@callback(
    Output('approval-table', 'src'),
    Input('switch', 'value')
)
def update_chart_color_mode(color_mode):
    if color_mode:
        return f"https://datawrapper.dwcdn.net/{APPROVAL_TABLE_KEY}/"
    else:
        return f"https://datawrapper.dwcdn.net/{APPROVAL_TABLE_KEY}/?dark=true"

@callback(
    Output('navbar', 'color'),
    Output('navbar', 'dark'),
    Input('switch', 'value')
)
def update_navbar_color_mode(color_mode):
    if color_mode:
        return "light", False
    else:
        return "dark", True

@callback(
    Output('logo', 'src'),
    Input('switch', 'value')
)
def update_logo_color_mode(color_mode):
    if color_mode:
        return "/assets/snoutcounter.png"
    else:
        return "/assets/snoutcounter_darkmode.png"


if __name__ == "__main__":
    app.run_server(debug=True)

