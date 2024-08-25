/*
 * This file is generated by jOOQ.
 */
package trash.tables.records;


import java.time.LocalDateTime;

import org.jooq.Record1;
import org.jooq.impl.UpdatableRecordImpl;
import org.jooq.types.ULong;

import trash.tables.Review;


/**
 * 리뷰 테이블
 */
@SuppressWarnings({ "all", "unchecked", "rawtypes", "this-escape" })
public class ReviewRecord extends UpdatableRecordImpl<ReviewRecord> {

    private static final long serialVersionUID = 1L;

    /**
     * Setter for <code>spotfinder.REVIEW.RVW_SEQ</code>. 리뷰 일련번호
     */
    public ReviewRecord setRvwSeq(ULong value) {
        set(0, value);
        return this;
    }

    /**
     * Getter for <code>spotfinder.REVIEW.RVW_SEQ</code>. 리뷰 일련번호
     */
    public ULong getRvwSeq() {
        return (ULong) get(0);
    }

    /**
     * Setter for <code>spotfinder.REVIEW.RVW_CTT</code>. 리뷰 내용
     */
    public ReviewRecord setRvwCtt(String value) {
        set(1, value);
        return this;
    }

    /**
     * Getter for <code>spotfinder.REVIEW.RVW_CTT</code>. 리뷰 내용
     */
    public String getRvwCtt() {
        return (String) get(1);
    }

    /**
     * Setter for <code>spotfinder.REVIEW.MBR_SEQ</code>. 회원 일련번호
     */
    public ReviewRecord setMbrSeq(ULong value) {
        set(2, value);
        return this;
    }

    /**
     * Getter for <code>spotfinder.REVIEW.MBR_SEQ</code>. 회원 일련번호
     */
    public ULong getMbrSeq() {
        return (ULong) get(2);
    }

    /**
     * Setter for <code>spotfinder.REVIEW.FCLTY_ID</code>. 시설물 ID
     */
    public ReviewRecord setFcltyId(String value) {
        set(3, value);
        return this;
    }

    /**
     * Getter for <code>spotfinder.REVIEW.FCLTY_ID</code>. 시설물 ID
     */
    public String getFcltyId() {
        return (String) get(3);
    }

    /**
     * Setter for <code>spotfinder.REVIEW.CRE_DTM</code>. 등록일시
     */
    public ReviewRecord setCreDtm(LocalDateTime value) {
        set(4, value);
        return this;
    }

    /**
     * Getter for <code>spotfinder.REVIEW.CRE_DTM</code>. 등록일시
     */
    public LocalDateTime getCreDtm() {
        return (LocalDateTime) get(4);
    }

    /**
     * Setter for <code>spotfinder.REVIEW.UPD_DTM</code>. 수정일시
     */
    public ReviewRecord setUpdDtm(LocalDateTime value) {
        set(5, value);
        return this;
    }

    /**
     * Getter for <code>spotfinder.REVIEW.UPD_DTM</code>. 수정일시
     */
    public LocalDateTime getUpdDtm() {
        return (LocalDateTime) get(5);
    }

    // -------------------------------------------------------------------------
    // Primary key information
    // -------------------------------------------------------------------------

    @Override
    public Record1<ULong> key() {
        return (Record1) super.key();
    }

    // -------------------------------------------------------------------------
    // Constructors
    // -------------------------------------------------------------------------

    /**
     * Create a detached ReviewRecord
     */
    public ReviewRecord() {
        super(Review.REVIEW);
    }

    /**
     * Create a detached, initialised ReviewRecord
     */
    public ReviewRecord(ULong rvwSeq, String rvwCtt, ULong mbrSeq, String fcltyId, LocalDateTime creDtm, LocalDateTime updDtm) {
        super(Review.REVIEW);

        setRvwSeq(rvwSeq);
        setRvwCtt(rvwCtt);
        setMbrSeq(mbrSeq);
        setFcltyId(fcltyId);
        setCreDtm(creDtm);
        setUpdDtm(updDtm);
        resetChangedOnNotNull();
    }

    /**
     * Create a detached, initialised ReviewRecord
     */
    public ReviewRecord(trash.tables.pojos.Review value) {
        super(Review.REVIEW);

        if (value != null) {
            setRvwSeq(value.getRvwSeq());
            setRvwCtt(value.getRvwCtt());
            setMbrSeq(value.getMbrSeq());
            setFcltyId(value.getFcltyId());
            setCreDtm(value.getCreDtm());
            setUpdDtm(value.getUpdDtm());
            resetChangedOnNotNull();
        }
    }
}
