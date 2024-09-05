/*
 * This file is generated by jOOQ.
 */
package trash;


import trash.tables.Admin;
import trash.tables.Member;
import trash.tables.Notice;
import trash.tables.Report;
import trash.tables.Review;
import trash.tables.Token;


/**
 * Convenience access to all tables in spotfinder.
 */
@SuppressWarnings({ "all", "unchecked", "rawtypes", "this-escape" })
public class Tables {

    /**
     * 관리자 테이블
     */
    public static final Admin ADMIN = Admin.ADMIN;

    /**
     * The table <code>spotfinder.MEMBER</code>.
     */
    public static final Member MEMBER = Member.MEMBER;

    /**
     * The table <code>spotfinder.NOTICE</code>.
     */
    public static final Notice NOTICE = Notice.NOTICE;

    /**
     * 신고 테이블
     */
    public static final Report REPORT = Report.REPORT;

    /**
     * 리뷰 테이블
     */
    public static final Review REVIEW = Review.REVIEW;

    /**
     * The table <code>spotfinder.TOKEN</code>.
     */
    public static final Token TOKEN = Token.TOKEN;
}
