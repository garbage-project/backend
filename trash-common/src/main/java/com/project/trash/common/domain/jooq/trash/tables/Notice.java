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
import trash.Spotfinder;
import trash.tables.records.NoticeRecord;


/**
 * This class is generated by jOOQ.
 */
@SuppressWarnings({ "all", "unchecked", "rawtypes", "this-escape" })
public class Notice extends TableImpl<NoticeRecord> {

    private static final long serialVersionUID = 1L;

    /**
     * The reference instance of <code>spotfinder.NOTICE</code>
     */
    public static final Notice NOTICE = new Notice();

    /**
     * The class holding records for this type
     */
    @Override
    public Class<NoticeRecord> getRecordType() {
        return NoticeRecord.class;
    }

    /**
     * The column <code>spotfinder.NOTICE.NTC_ID</code>. 공지 ID
     */
    public final TableField<NoticeRecord, ULong> NTC_ID = createField(DSL.name("NTC_ID"), SQLDataType.BIGINTUNSIGNED.nullable(false).identity(true), this, "공지 ID");

    /**
     * The column <code>spotfinder.NOTICE.NTC_TTL</code>. 공지 제목
     */
    public final TableField<NoticeRecord, String> NTC_TTL = createField(DSL.name("NTC_TTL"), SQLDataType.VARCHAR(50).nullable(false), this, "공지 제목");

    /**
     * The column <code>spotfinder.NOTICE.NTC_CTT</code>. 공지 내용
     */
    public final TableField<NoticeRecord, String> NTC_CTT = createField(DSL.name("NTC_CTT"), SQLDataType.VARCHAR(255).nullable(false), this, "공지 내용");

    /**
     * The column <code>spotfinder.NOTICE.NTC_VLD_YN</code>. 유효여부
     */
    public final TableField<NoticeRecord, String> NTC_VLD_YN = createField(DSL.name("NTC_VLD_YN"), SQLDataType.CHAR(1).nullable(false), this, "유효여부");

    /**
     * The column <code>spotfinder.NOTICE.CRE_DTM</code>. 등록일시
     */
    public final TableField<NoticeRecord, LocalDateTime> CRE_DTM = createField(DSL.name("CRE_DTM"), SQLDataType.LOCALDATETIME(0).nullable(false), this, "등록일시");

    /**
     * The column <code>spotfinder.NOTICE.UPD_DTM</code>. 수정일시
     */
    public final TableField<NoticeRecord, LocalDateTime> UPD_DTM = createField(DSL.name("UPD_DTM"), SQLDataType.LOCALDATETIME(0).nullable(false), this, "수정일시");

    private Notice(Name alias, Table<NoticeRecord> aliased) {
        this(alias, aliased, null);
    }

    private Notice(Name alias, Table<NoticeRecord> aliased, Field<?>[] parameters) {
        super(alias, null, aliased, parameters, DSL.comment(""), TableOptions.table());
    }

    /**
     * Create an aliased <code>spotfinder.NOTICE</code> table reference
     */
    public Notice(String alias) {
        this(DSL.name(alias), NOTICE);
    }

    /**
     * Create an aliased <code>spotfinder.NOTICE</code> table reference
     */
    public Notice(Name alias) {
        this(alias, NOTICE);
    }

    /**
     * Create a <code>spotfinder.NOTICE</code> table reference
     */
    public Notice() {
        this(DSL.name("NOTICE"), null);
    }

    public <O extends Record> Notice(Table<O> child, ForeignKey<O, NoticeRecord> key) {
        super(child, key, NOTICE);
    }

    @Override
    public Schema getSchema() {
        return aliased() ? null : Spotfinder.SPOTFINDER;
    }

    @Override
    public Identity<NoticeRecord, ULong> getIdentity() {
        return (Identity<NoticeRecord, ULong>) super.getIdentity();
    }

    @Override
    public UniqueKey<NoticeRecord> getPrimaryKey() {
        return Keys.KEY_NOTICE_PRIMARY;
    }

    @Override
    public Notice as(String alias) {
        return new Notice(DSL.name(alias), this);
    }

    @Override
    public Notice as(Name alias) {
        return new Notice(alias, this);
    }

    @Override
    public Notice as(Table<?> alias) {
        return new Notice(alias.getQualifiedName(), this);
    }

    /**
     * Rename this table
     */
    @Override
    public Notice rename(String name) {
        return new Notice(DSL.name(name), null);
    }

    /**
     * Rename this table
     */
    @Override
    public Notice rename(Name name) {
        return new Notice(name, null);
    }

    /**
     * Rename this table
     */
    @Override
    public Notice rename(Table<?> name) {
        return new Notice(name.getQualifiedName(), null);
    }
}
