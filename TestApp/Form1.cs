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
using org.bovinegenius.DataBeast.Expression;

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
            Expression<Func<Object>> orig_exp = () =>
               (from m in db["movies"]
                //from x in db["asdf"]
                from y in db["cool"]
                join x in db["other stuff"] on m["id"] equals x["MovieID"]
                //join y in db["ymovies"] on x["MovieID"] equals y["id"] 
                where m["movieid"] == id && m["title"] == "Batman Begins"
                orderby m["stuff"] descending, x["y"]
                //orderby m["thing"] descending
                select new { m, x }).Where(p => p.m["key"] == "stuff").Where(a => a.x["bob"] == null);

            var exp = ((LambdaExpression)Translate.collapse_where(Eval.partial_eval(orig_exp))).Body; //Translate.strip_quotes(orig_exp.Body);
            Linq.Text = org.bovinegenius.DataBeast.PrintExpression.print_exp(exp);

            try {
                var query = org.bovinegenius.DataBeast.Expression.Translate.to_query(exp);
                var args = org.bovinegenius.DataBeast.Sql.query_to_constants(query);
                Sql.Text = org.bovinegenius.DataBeast.Serialize.to_mysql(query);
                Sql.Text += "\r\n\r\n" + args.ToString();
            } catch (Exception ex) {
                Sql.Text = ex.ToString();
            }
        }
    }
}
