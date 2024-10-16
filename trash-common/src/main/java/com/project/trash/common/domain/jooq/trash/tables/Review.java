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
import trash.tables.records.ReviewRecord;


/**
 * 리뷰 테이블
 */
@SuppressWarnings({ "all", "unchecked", "rawtypes", "this-escape" })
public class Review extends TableImpl<ReviewRecord> {

    private static final long serialVersionUID = 1L;

    /**
     * The reference instance of <code>spotfinder.REVIEW</code>
     */
    public static final Review REVIEW = new Review();

    /**
     * The class holding records for this type
     */
    @Override
    public Class<ReviewRecord> getRecordType() {
        return ReviewRecord.class;
    }

    /**
     * The column <code>spotfinder.REVIEW.RVW_ID</code>. 리뷰 ID
     */
    public final TableField<ReviewRecord, ULong> RVW_ID = createField(DSL.name("RVW_ID"), SQLDataType.BIGINTUNSIGNED.nullable(false).identity(true), this, "리뷰 ID");

    /**
     * The column <code>spotfinder.REVIEW.RVW_CTT</code>. 리뷰 내용
     */
    public final TableField<ReviewRecord, String> RVW_CTT = createField(DSL.name("RVW_CTT"), SQLDataType.VARCHAR(100).nullable(false), this, "리뷰 내용");

    /**
     * The column <code>spotfinder.REVIEW.MBR_ID</code>. 회원 ID
     */
    public final TableField<ReviewRecord, ULong> MBR_ID = createField(DSL.name("MBR_ID"), SQLDataType.BIGINTUNSIGNED.nullable(false), this, "회원 ID");

    /**
     * The column <code>spotfinder.REVIEW.FCLTY_ID</code>. 시설물 ID
     */
    public final TableField<ReviewRecord, ULong> FCLTY_ID = createField(DSL.name("FCLTY_ID"), SQLDataType.BIGINTUNSIGNED.nullable(false), this, "시설물 ID");

    /**
     * The column <code>spotfinder.REVIEW.CRE_DTM</code>. 등록일시
     */
    public final TableField<ReviewRecord, LocalDateTime> CRE_DTM = createField(DSL.name("CRE_DTM"), SQLDataType.LOCALDATETIME(0).nullable(false), this, "등록일시");

    /**
     * The column <code>spotfinder.REVIEW.UPD_DTM</code>. 수정일시
     */
    public final TableField<ReviewRecord, LocalDateTime> UPD_DTM = createField(DSL.name("UPD_DTM"), SQLDataType.LOCALDATETIME(0).nullable(false), this, "수정일시");

    private Review(Name alias, Table<ReviewRecord> aliased) {
        this(alias, aliased, null);
    }

    private Review(Name alias, Table<ReviewRecord> aliased, Field<?>[] parameters) {
        super(alias, null, aliased, parameters, DSL.comment("리뷰 테이블"), TableOptions.table());
    }

    /**
     * Create an aliased <code>spotfinder.REVIEW</code> table reference
     */
    public Review(String alias) {
        this(DSL.name(alias), REVIEW);
    }

    /**
     * Create an aliased <code>spotfinder.REVIEW</code> table reference
     */
    public Review(Name alias) {
        this(alias, REVIEW);
    }

    /**
     * Create a <code>spotfinder.REVIEW</code> table reference
     */
    public Review() {
        this(DSL.name("REVIEW"), null);
    }

    public <O extends Record> Review(Table<O> child, ForeignKey<O, ReviewRecord> key) {
        super(child, key, REVIEW);
    }

    @Override
    public Schema getSchema() {
        return aliased() ? null : Spotfinder.SPOTFINDER;
    }

    @Override
    public Identity<ReviewRecord, ULong> getIdentity() {
        return (Identity<ReviewRecord, ULong>) super.getIdentity();
    }

    @Override
    public UniqueKey<ReviewRecord> getPrimaryKey() {
        return Keys.KEY_REVIEW_PRIMARY;
    }

    @Override
    public Review as(String alias) {
        return new Review(DSL.name(alias), this);
    }

    @Override
    public Review as(Name alias) {
        return new Review(alias, this);
    }

    @Override
    public Review as(Table<?> alias) {
        return new Review(alias.getQualifiedName(), this);
    }

    /**
     * Rename this table
     */
    @Override
    public Review rename(String name) {
        return new Review(DSL.name(name), null);
    }

    /**
     * Rename this table
     */
    @Override
    public Review rename(Name name) {
        return new Review(name, null);
    }

    /**
     * Rename this table
     */
    @Override
    public Review rename(Table<?> name) {
        return new Review(name.getQualifiedName(), null);
    }
}
