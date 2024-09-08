/*
 * This file is generated by jOOQ.
 */
package trash.tables.pojos;


import java.io.Serializable;
import java.time.LocalDateTime;

import org.jooq.types.ULong;


/**
 * 리뷰 테이블
 */
@SuppressWarnings({ "all", "unchecked", "rawtypes", "this-escape" })
public class Review implements Serializable {

    private static final long serialVersionUID = 1L;

    private final ULong rvwSeq;
    private final String rvwCtt;
    private final ULong mbrSeq;
    private final ULong fcltySeq;
    private final LocalDateTime creDtm;
    private final LocalDateTime updDtm;
    private final String rptSttYn;

    public Review(Review value) {
        this.rvwSeq = value.rvwSeq;
        this.rvwCtt = value.rvwCtt;
        this.mbrSeq = value.mbrSeq;
        this.fcltySeq = value.fcltySeq;
        this.creDtm = value.creDtm;
        this.updDtm = value.updDtm;
        this.rptSttYn = value.rptSttYn;
    }

    public Review(
        ULong rvwSeq,
        String rvwCtt,
        ULong mbrSeq,
        ULong fcltySeq,
        LocalDateTime creDtm,
        LocalDateTime updDtm,
        String rptSttYn
    ) {
        this.rvwSeq = rvwSeq;
        this.rvwCtt = rvwCtt;
        this.mbrSeq = mbrSeq;
        this.fcltySeq = fcltySeq;
        this.creDtm = creDtm;
        this.updDtm = updDtm;
        this.rptSttYn = rptSttYn;
    }

    /**
     * Getter for <code>spotfinder.REVIEW.RVW_SEQ</code>. 리뷰 일련번호
     */
    public ULong getRvwSeq() {
        return this.rvwSeq;
    }

    /**
     * Getter for <code>spotfinder.REVIEW.RVW_CTT</code>. 리뷰 내용
     */
    public String getRvwCtt() {
        return this.rvwCtt;
    }

    /**
     * Getter for <code>spotfinder.REVIEW.MBR_SEQ</code>. 회원 일련번호
     */
    public ULong getMbrSeq() {
        return this.mbrSeq;
    }

    /**
     * Getter for <code>spotfinder.REVIEW.FCLTY_SEQ</code>. 시설물 일련번호
     */
    public ULong getFcltySeq() {
        return this.fcltySeq;
    }

    /**
     * Getter for <code>spotfinder.REVIEW.CRE_DTM</code>. 등록일시
     */
    public LocalDateTime getCreDtm() {
        return this.creDtm;
    }

    /**
     * Getter for <code>spotfinder.REVIEW.UPD_DTM</code>. 수정일시
     */
    public LocalDateTime getUpdDtm() {
        return this.updDtm;
    }

    /**
     * Getter for <code>spotfinder.REVIEW.RPT_STT_YN</code>. 신고 처리상태
     */
    public String getRptSttYn() {
        return this.rptSttYn;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        final Review other = (Review) obj;
        if (this.rvwSeq == null) {
            if (other.rvwSeq != null)
                return false;
        }
        else if (!this.rvwSeq.equals(other.rvwSeq))
            return false;
        if (this.rvwCtt == null) {
            if (other.rvwCtt != null)
                return false;
        }
        else if (!this.rvwCtt.equals(other.rvwCtt))
            return false;
        if (this.mbrSeq == null) {
            if (other.mbrSeq != null)
                return false;
        }
        else if (!this.mbrSeq.equals(other.mbrSeq))
            return false;
        if (this.fcltySeq == null) {
            if (other.fcltySeq != null)
                return false;
        }
        else if (!this.fcltySeq.equals(other.fcltySeq))
            return false;
        if (this.creDtm == null) {
            if (other.creDtm != null)
                return false;
        }
        else if (!this.creDtm.equals(other.creDtm))
            return false;
        if (this.updDtm == null) {
            if (other.updDtm != null)
                return false;
        }
        else if (!this.updDtm.equals(other.updDtm))
            return false;
        if (this.rptSttYn == null) {
            if (other.rptSttYn != null)
                return false;
        }
        else if (!this.rptSttYn.equals(other.rptSttYn))
            return false;
        return true;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((this.rvwSeq == null) ? 0 : this.rvwSeq.hashCode());
        result = prime * result + ((this.rvwCtt == null) ? 0 : this.rvwCtt.hashCode());
        result = prime * result + ((this.mbrSeq == null) ? 0 : this.mbrSeq.hashCode());
        result = prime * result + ((this.fcltySeq == null) ? 0 : this.fcltySeq.hashCode());
        result = prime * result + ((this.creDtm == null) ? 0 : this.creDtm.hashCode());
        result = prime * result + ((this.updDtm == null) ? 0 : this.updDtm.hashCode());
        result = prime * result + ((this.rptSttYn == null) ? 0 : this.rptSttYn.hashCode());
        return result;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("Review (");

        sb.append(rvwSeq);
        sb.append(", ").append(rvwCtt);
        sb.append(", ").append(mbrSeq);
        sb.append(", ").append(fcltySeq);
        sb.append(", ").append(creDtm);
        sb.append(", ").append(updDtm);
        sb.append(", ").append(rptSttYn);

        sb.append(")");
        return sb.toString();
    }
}
