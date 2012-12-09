using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Linq.Expressions;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using org.bovinegenius.DataBeast;
using Roslyn.Compilers.CSharp;

namespace TestApp
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();
        }

        private void Translate_Click(object sender, EventArgs e)
        {
            var db = new org.bovinegenius.DataBeast.Database(org.bovinegenius.DataBeast.Dbms.MySql, "connection string");
            var id = "some-id";
            Expression<Func<Object>> exp = () =>
                from m in db["movies"]
                join x in db["other stuff"] on m["id"] equals x["MovieID"]
                where m["movieid"] == id && m["title"] == "Batman Begins"
                orderby m["stuff"] descending
                select m;
            Linq.Text = org.bovinegenius.DataBeast.PrintExpression.print_exp(exp);

            try {
                var query = org.bovinegenius.DataBeast.Expression.Translate.to_query(exp.Body);
                var args = org.bovinegenius.DataBeast.Sql.query_to_constants(query);
                Sql.Text = org.bovinegenius.DataBeast.Serialize.to_mysql(query);
                Sql.Text += "\r\n\r\n" + args.ToString();
            } catch (Exception ex) {
                Sql.Text = ex.ToString();
            }
        }
    }
}
