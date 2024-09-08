package com.project.trash.facility.dao;

import com.project.trash.member.request.MemberFacilityListRequest;
import com.project.trash.member.response.MemberFacilityListResponse;

import org.jooq.DSLContext;
import org.jooq.types.ULong;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Repository;

import java.util.List;

import lombok.RequiredArgsConstructor;

import static trash.Tables.REVIEW;
import static trash.tables.Facility.FACILITY;

/**
 * 시설물 조회 DAO
 */
@Repository
@RequiredArgsConstructor
public class FacilityDao {

  private final DSLContext dsl;

  /**
   * 등록한 시설물 목록 조회 총개수
   */
  public Long count(Long memberSeq) {
    return dsl.selectCount()
              .from(FACILITY)
              .where(FACILITY.MBR_SEQ.eq(ULong.valueOf(memberSeq)))
              .fetchOneInto(Long.class);
  }

  /**
   * 등록한 시설물 목록 조회
   */
  public List<MemberFacilityListResponse> select(MemberFacilityListRequest param) {
    return dsl.select(FACILITY.FCLTY_SEQ, FACILITY.FCLTY_TYP, FACILITY.FCLTY_NM, FACILITY.FCLTY_LCTN,
                  FACILITY.FCLTY_DTL_LCTN, FACILITY.FCLTY_INFO, FACILITY.FCLTY_APRV_YN)
              .from(FACILITY)
              .where(FACILITY.MBR_SEQ.eq(ULong.valueOf(param.getMemberSeq())))
              .orderBy(FACILITY.CRE_DTM.desc())
              .limit(param.getSize())
              .offset(param.getOffset())
              .fetchInto(MemberFacilityListResponse.class);
  }
}
