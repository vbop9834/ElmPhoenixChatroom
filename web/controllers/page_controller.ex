defmodule Testelm.PageController do
  use Testelm.Web, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
