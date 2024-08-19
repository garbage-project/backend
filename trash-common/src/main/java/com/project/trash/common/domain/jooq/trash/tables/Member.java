/*
 * This file is generated by jOOQ.
 */
package trash.tables;


import java.time.LocalDateTime;

import org.jooq.Field;
import org.jooq.ForeignKey;
import org.jooq.Identity;
import org.jooq.Name;
import org.jooq.Record;
import org.jooq.Schema;
import org.jooq.Table;
import org.jooq.TableField;
import org.jooq.TableOptions;
import org.jooq.UniqueKey;
import org.jooq.impl.DSL;
import org.jooq.impl.SQLDataType;
import org.jooq.impl.TableImpl;
import org.jooq.types.ULong;

import trash.Keys;
import trash.Trash;
import trash.tables.records.MemberRecord;


/**
 * This class is generated by jOOQ.
 */
@SuppressWarnings({ "all", "unchecked", "rawtypes", "this-escape" })
public class Member extends TableImpl<MemberRecord> {

    private static final long serialVersionUID = 1L;

    /**
     * The reference instance of <code>trash.MEMBER</code>
     */
    public static final Member MEMBER = new Member();

    /**
     * The class holding records for this type
     */
    @Override
    public Class<MemberRecord> getRecordType() {
        return MemberRecord.class;
    }

    /**
     * The column <code>trash.MEMBER.MBR_SEQ</code>. 회원 일련번호
     */
    public final TableField<MemberRecord, ULong> MBR_SEQ = createField(DSL.name("MBR_SEQ"), SQLDataType.BIGINTUNSIGNED.nullable(false).identity(true), this, "회원 일련번호");

    /**
     * The column <code>trash.MEMBER.MBR_EMAIL</code>. 이메일
     */
    public final TableField<MemberRecord, String> MBR_EMAIL = createField(DSL.name("MBR_EMAIL"), SQLDataType.VARCHAR(50).nullable(false), this, "이메일");

    /**
     * The column <code>trash.MEMBER.MBR_NM</code>. 이름
     */
    public final TableField<MemberRecord, String> MBR_NM = createField(DSL.name("MBR_NM"), SQLDataType.VARCHAR(10).nullable(false), this, "이름");

    /**
     * The column <code>trash.MEMBER.MBR_NCK_NM</code>. 닉네임
     */
    public final TableField<MemberRecord, String> MBR_NCK_NM = createField(DSL.name("MBR_NCK_NM"), SQLDataType.VARCHAR(20), this, "닉네임");

    /**
     * The column <code>trash.MEMBER.MBR_GNDR</code>. 성별
     */
    public final TableField<MemberRecord, String> MBR_GNDR = createField(DSL.name("MBR_GNDR"), SQLDataType.CHAR(1).nullable(false), this, "성별");

    /**
     * The column <code>trash.MEMBER.MBR_BRDT</code>. 생년월일
     */
    public final TableField<MemberRecord, String> MBR_BRDT = createField(DSL.name("MBR_BRDT"), SQLDataType.VARCHAR(8).nullable(false), this, "생년월일");

    /**
     * The column <code>trash.MEMBER.MBR_SCL_ID</code>. 소셜 ID
     */
    public final TableField<MemberRecord, String> MBR_SCL_ID = createField(DSL.name("MBR_SCL_ID"), SQLDataType.VARCHAR(50).nullable(false), this, "소셜 ID");

    /**
     * The column <code>trash.MEMBER.MBR_SCL_TYP</code>. 소셜 타입
     */
    public final TableField<MemberRecord, String> MBR_SCL_TYP = createField(DSL.name("MBR_SCL_TYP"), SQLDataType.CHAR(1).nullable(false), this, "소셜 타입");

    /**
     * The column <code>trash.MEMBER.MBR_VLD_YN</code>. 유효여부
     */
    public final TableField<MemberRecord, String> MBR_VLD_YN = createField(DSL.name("MBR_VLD_YN"), SQLDataType.CHAR(1).nullable(false), this, "유효여부");

    /**
     * The column <code>trash.MEMBER.CRE_DTM</code>. 등록일시
     */
    public final TableField<MemberRecord, LocalDateTime> CRE_DTM = createField(DSL.name("CRE_DTM"), SQLDataType.LOCALDATETIME(0).nullable(false), this, "등록일시");

    /**
     * The column <code>trash.MEMBER.UPD_DTM</code>. 수정일시
     */
    public final TableField<MemberRecord, LocalDateTime> UPD_DTM = createField(DSL.name("UPD_DTM"), SQLDataType.LOCALDATETIME(0).nullable(false), this, "수정일시");

    private Member(Name alias, Table<MemberRecord> aliased) {
        this(alias, aliased, null);
    }

    private Member(Name alias, Table<MemberRecord> aliased, Field<?>[] parameters) {
        super(alias, null, aliased, parameters, DSL.comment(""), TableOptions.table());
    }

    /**
     * Create an aliased <code>trash.MEMBER</code> table reference
     */
    public Member(String alias) {
        this(DSL.name(alias), MEMBER);
    }

    /**
     * Create an aliased <code>trash.MEMBER</code> table reference
     */
    public Member(Name alias) {
        this(alias, MEMBER);
    }

    /**
     * Create a <code>trash.MEMBER</code> table reference
     */
    public Member() {
        this(DSL.name("MEMBER"), null);
    }

    public <O extends Record> Member(Table<O> child, ForeignKey<O, MemberRecord> key) {
        super(child, key, MEMBER);
    }

    @Override
    public Schema getSchema() {
        return aliased() ? null : Trash.TRASH;
    }

    @Override
    public Identity<MemberRecord, ULong> getIdentity() {
        return (Identity<MemberRecord, ULong>) super.getIdentity();
    }

    @Override
    public UniqueKey<MemberRecord> getPrimaryKey() {
        return Keys.KEY_MEMBER_PRIMARY;
    }

    @Override
    public Member as(String alias) {
        return new Member(DSL.name(alias), this);
    }

    @Override
    public Member as(Name alias) {
        return new Member(alias, this);
    }

    @Override
    public Member as(Table<?> alias) {
        return new Member(alias.getQualifiedName(), this);
    }

    /**
     * Rename this table
     */
    @Override
    public Member rename(String name) {
        return new Member(DSL.name(name), null);
    }

    /**
     * Rename this table
     */
    @Override
    public Member rename(Name name) {
        return new Member(name, null);
    }

    /**
     * Rename this table
     */
    @Override
    public Member rename(Table<?> name) {
        return new Member(name.getQualifiedName(), null);
    }
}
