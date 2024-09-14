/*
 * This file is generated by jOOQ.
 */
package trash.tables.pojos;


import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;

import org.jooq.types.ULong;


/**
 * 시설물 테이블
 */
@SuppressWarnings({ "all", "unchecked", "rawtypes", "this-escape" })
public class Facility implements Serializable {

    private static final long serialVersionUID = 1L;

    private final ULong fcltyId;
    private final String fcltyNm;
    private final String fcltyTyp;
    private final String fcltyLctn;
    private final String fcltyDtlLctn;
    private final BigDecimal fcltyLttd;
    private final BigDecimal fcltyLngt;
    private final String fcltyInfo;
    private final String fcltyDprNm;
    private final String fcltyDprTlphNmbr;
    private final String fcltyAprvSta;
    private final String mbrId;
    private final LocalDateTime creDtm;
    private final LocalDateTime updDtm;

    public Facility(Facility value) {
        this.fcltyId = value.fcltyId;
        this.fcltyNm = value.fcltyNm;
        this.fcltyTyp = value.fcltyTyp;
        this.fcltyLctn = value.fcltyLctn;
        this.fcltyDtlLctn = value.fcltyDtlLctn;
        this.fcltyLttd = value.fcltyLttd;
        this.fcltyLngt = value.fcltyLngt;
        this.fcltyInfo = value.fcltyInfo;
        this.fcltyDprNm = value.fcltyDprNm;
        this.fcltyDprTlphNmbr = value.fcltyDprTlphNmbr;
        this.fcltyAprvSta = value.fcltyAprvSta;
        this.mbrId = value.mbrId;
        this.creDtm = value.creDtm;
        this.updDtm = value.updDtm;
    }

    public Facility(
        ULong fcltyId,
        String fcltyNm,
        String fcltyTyp,
        String fcltyLctn,
        String fcltyDtlLctn,
        BigDecimal fcltyLttd,
        BigDecimal fcltyLngt,
        String fcltyInfo,
        String fcltyDprNm,
        String fcltyDprTlphNmbr,
        String fcltyAprvSta,
        String mbrId,
        LocalDateTime creDtm,
        LocalDateTime updDtm
    ) {
        this.fcltyId = fcltyId;
        this.fcltyNm = fcltyNm;
        this.fcltyTyp = fcltyTyp;
        this.fcltyLctn = fcltyLctn;
        this.fcltyDtlLctn = fcltyDtlLctn;
        this.fcltyLttd = fcltyLttd;
        this.fcltyLngt = fcltyLngt;
        this.fcltyInfo = fcltyInfo;
        this.fcltyDprNm = fcltyDprNm;
        this.fcltyDprTlphNmbr = fcltyDprTlphNmbr;
        this.fcltyAprvSta = fcltyAprvSta;
        this.mbrId = mbrId;
        this.creDtm = creDtm;
        this.updDtm = updDtm;
    }

    /**
     * Getter for <code>spotfinder.FACILITY.FCLTY_ID</code>. 시설물 ID
     */
    public ULong getFcltyId() {
        return this.fcltyId;
    }

    /**
     * Getter for <code>spotfinder.FACILITY.FCLTY_NM</code>. 시설물명
     */
    public String getFcltyNm() {
        return this.fcltyNm;
    }

    /**
     * Getter for <code>spotfinder.FACILITY.FCLTY_TYP</code>. 시설물 종류
     */
    public String getFcltyTyp() {
        return this.fcltyTyp;
    }

    /**
     * Getter for <code>spotfinder.FACILITY.FCLTY_LCTN</code>. 위치
     */
    public String getFcltyLctn() {
        return this.fcltyLctn;
    }

    /**
     * Getter for <code>spotfinder.FACILITY.FCLTY_DTL_LCTN</code>. 상세 위치
     */
    public String getFcltyDtlLctn() {
        return this.fcltyDtlLctn;
    }

    /**
     * Getter for <code>spotfinder.FACILITY.FCLTY_LTTD</code>. 위도
     */
    public BigDecimal getFcltyLttd() {
        return this.fcltyLttd;
    }

    /**
     * Getter for <code>spotfinder.FACILITY.FCLTY_LNGT</code>. 경도
     */
    public BigDecimal getFcltyLngt() {
        return this.fcltyLngt;
    }

    /**
     * Getter for <code>spotfinder.FACILITY.FCLTY_INFO</code>. 정보
     */
    public String getFcltyInfo() {
        return this.fcltyInfo;
    }

    /**
     * Getter for <code>spotfinder.FACILITY.FCLTY_DPR_NM</code>. 관리 부서명
     */
    public String getFcltyDprNm() {
        return this.fcltyDprNm;
    }

    /**
     * Getter for <code>spotfinder.FACILITY.FCLTY_DPR_TLPH_NMBR</code>. 관리 부서
     * 전화번호
     */
    public String getFcltyDprTlphNmbr() {
        return this.fcltyDprTlphNmbr;
    }

    /**
     * Getter for <code>spotfinder.FACILITY.FCLTY_APRV_STA</code>. 승인 상태
     */
    public String getFcltyAprvSta() {
        return this.fcltyAprvSta;
    }

    /**
     * Getter for <code>spotfinder.FACILITY.MBR_ID</code>. 회원 ID
     */
    public String getMbrId() {
        return this.mbrId;
    }

    /**
     * Getter for <code>spotfinder.FACILITY.CRE_DTM</code>. 등록일시
     */
    public LocalDateTime getCreDtm() {
        return this.creDtm;
    }

    /**
     * Getter for <code>spotfinder.FACILITY.UPD_DTM</code>. 수정일시
     */
    public LocalDateTime getUpdDtm() {
        return this.updDtm;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        final Facility other = (Facility) obj;
        if (this.fcltyId == null) {
            if (other.fcltyId != null)
                return false;
        }
        else if (!this.fcltyId.equals(other.fcltyId))
            return false;
        if (this.fcltyNm == null) {
            if (other.fcltyNm != null)
                return false;
        }
        else if (!this.fcltyNm.equals(other.fcltyNm))
            return false;
        if (this.fcltyTyp == null) {
            if (other.fcltyTyp != null)
                return false;
        }
        else if (!this.fcltyTyp.equals(other.fcltyTyp))
            return false;
        if (this.fcltyLctn == null) {
            if (other.fcltyLctn != null)
                return false;
        }
        else if (!this.fcltyLctn.equals(other.fcltyLctn))
            return false;
        if (this.fcltyDtlLctn == null) {
            if (other.fcltyDtlLctn != null)
                return false;
        }
        else if (!this.fcltyDtlLctn.equals(other.fcltyDtlLctn))
            return false;
        if (this.fcltyLttd == null) {
            if (other.fcltyLttd != null)
                return false;
        }
        else if (!this.fcltyLttd.equals(other.fcltyLttd))
            return false;
        if (this.fcltyLngt == null) {
            if (other.fcltyLngt != null)
                return false;
        }
        else if (!this.fcltyLngt.equals(other.fcltyLngt))
            return false;
        if (this.fcltyInfo == null) {
            if (other.fcltyInfo != null)
                return false;
        }
        else if (!this.fcltyInfo.equals(other.fcltyInfo))
            return false;
        if (this.fcltyDprNm == null) {
            if (other.fcltyDprNm != null)
                return false;
        }
        else if (!this.fcltyDprNm.equals(other.fcltyDprNm))
            return false;
        if (this.fcltyDprTlphNmbr == null) {
            if (other.fcltyDprTlphNmbr != null)
                return false;
        }
        else if (!this.fcltyDprTlphNmbr.equals(other.fcltyDprTlphNmbr))
            return false;
        if (this.fcltyAprvSta == null) {
            if (other.fcltyAprvSta != null)
                return false;
        }
        else if (!this.fcltyAprvSta.equals(other.fcltyAprvSta))
            return false;
        if (this.mbrId == null) {
            if (other.mbrId != null)
                return false;
        }
        else if (!this.mbrId.equals(other.mbrId))
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
        return true;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((this.fcltyId == null) ? 0 : this.fcltyId.hashCode());
        result = prime * result + ((this.fcltyNm == null) ? 0 : this.fcltyNm.hashCode());
        result = prime * result + ((this.fcltyTyp == null) ? 0 : this.fcltyTyp.hashCode());
        result = prime * result + ((this.fcltyLctn == null) ? 0 : this.fcltyLctn.hashCode());
        result = prime * result + ((this.fcltyDtlLctn == null) ? 0 : this.fcltyDtlLctn.hashCode());
        result = prime * result + ((this.fcltyLttd == null) ? 0 : this.fcltyLttd.hashCode());
        result = prime * result + ((this.fcltyLngt == null) ? 0 : this.fcltyLngt.hashCode());
        result = prime * result + ((this.fcltyInfo == null) ? 0 : this.fcltyInfo.hashCode());
        result = prime * result + ((this.fcltyDprNm == null) ? 0 : this.fcltyDprNm.hashCode());
        result = prime * result + ((this.fcltyDprTlphNmbr == null) ? 0 : this.fcltyDprTlphNmbr.hashCode());
        result = prime * result + ((this.fcltyAprvSta == null) ? 0 : this.fcltyAprvSta.hashCode());
        result = prime * result + ((this.mbrId == null) ? 0 : this.mbrId.hashCode());
        result = prime * result + ((this.creDtm == null) ? 0 : this.creDtm.hashCode());
        result = prime * result + ((this.updDtm == null) ? 0 : this.updDtm.hashCode());
        return result;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("Facility (");

        sb.append(fcltyId);
        sb.append(", ").append(fcltyNm);
        sb.append(", ").append(fcltyTyp);
        sb.append(", ").append(fcltyLctn);
        sb.append(", ").append(fcltyDtlLctn);
        sb.append(", ").append(fcltyLttd);
        sb.append(", ").append(fcltyLngt);
        sb.append(", ").append(fcltyInfo);
        sb.append(", ").append(fcltyDprNm);
        sb.append(", ").append(fcltyDprTlphNmbr);
        sb.append(", ").append(fcltyAprvSta);
        sb.append(", ").append(mbrId);
        sb.append(", ").append(creDtm);
        sb.append(", ").append(updDtm);

        sb.append(")");
        return sb.toString();
    }
}
