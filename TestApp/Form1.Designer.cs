﻿namespace TestApp
{
    partial class Form1
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.Linq = new System.Windows.Forms.TextBox();
            this.Sql = new System.Windows.Forms.TextBox();
            this.DoTranslate = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // Linq
            // 
            this.Linq.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.Linq.Font = new System.Drawing.Font("Consolas", 10.93194F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.Linq.Location = new System.Drawing.Point(12, 12);
            this.Linq.Multiline = true;
            this.Linq.Name = "Linq";
            this.Linq.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
            this.Linq.Size = new System.Drawing.Size(948, 337);
            this.Linq.TabIndex = 0;
            // 
            // Sql
            // 
            this.Sql.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.Sql.Font = new System.Drawing.Font("Consolas", 10.93194F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.Sql.Location = new System.Drawing.Point(12, 460);
            this.Sql.Multiline = true;
            this.Sql.Name = "Sql";
            this.Sql.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
            this.Sql.Size = new System.Drawing.Size(948, 371);
            this.Sql.TabIndex = 1;
            // 
            // DoTranslate
            // 
            this.DoTranslate.Location = new System.Drawing.Point(12, 377);
            this.DoTranslate.Name = "DoTranslate";
            this.DoTranslate.Size = new System.Drawing.Size(212, 56);
            this.DoTranslate.TabIndex = 2;
            this.DoTranslate.Text = "Translate";
            this.DoTranslate.UseVisualStyleBackColor = true;
            this.DoTranslate.Click += new System.EventHandler(this.Translate_Click);
            // 
            // Form1
            // 
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Inherit;
            this.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.ClientSize = new System.Drawing.Size(974, 843);
            this.Controls.Add(this.DoTranslate);
            this.Controls.Add(this.Sql);
            this.Controls.Add(this.Linq);
            this.Name = "Form1";
            this.Text = "Form1";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.TextBox Linq;
        private System.Windows.Forms.TextBox Sql;
        private System.Windows.Forms.Button DoTranslate;
    }
}

