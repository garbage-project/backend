package com.project.trash.facility.dao;

import com.project.trash.common.utils.DateTimeUtils;
import com.project.trash.facility.request.FacilityListRequest;
import com.project.trash.facility.response.FacilityListResponse;
import com.project.trash.member.request.MemberFacilityListRequest;
import com.project.trash.member.response.MemberFacilityListResponse;
import com.project.trash.member.response.MemberListResponse;

import org.apache.commons.lang3.StringUtils;
import org.jooq.Condition;
import org.jooq.DSLContext;
import org.jooq.impl.DSL;
import org.jooq.types.ULong;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.List;

import lombok.RequiredArgsConstructor;

import static trash.Tables.MEMBER;
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
              .where(FACILITY.MBR_ID.eq(String.valueOf(memberSeq)))
              .fetchOneInto(Long.class);
  }

  /**
   * 등록한 시설물 목록 조회
   */
  public List<MemberFacilityListResponse> select(MemberFacilityListRequest param) {
    return dsl.select(FACILITY.FCLTY_SEQ, FACILITY.FCLTY_TYP, FACILITY.FCLTY_NM, FACILITY.FCLTY_LCTN,
                  FACILITY.FCLTY_DTL_LCTN, FACILITY.FCLTY_INFO, FACILITY.FCLTY_APRV_STA)
              .from(FACILITY)
              .where(FACILITY.MBR_ID.eq(String.valueOf(param.getMemberSeq())))
              .orderBy(FACILITY.CRE_DTM.desc())
              .limit(param.getSize())
              .offset(param.getOffset())
              .fetchInto(MemberFacilityListResponse.class);
  }

  /**
   * 시설물 목록 조회 총개수
   */
  public Long count(FacilityListRequest param) {
    return dsl.selectCount()
              .from(FACILITY)
              .where(getConditions(param))
              .fetchOneInto(Long.class);
  }

  /**
   * 시설물 목록 조회
   */
  public List<FacilityListResponse> select(FacilityListRequest param) {
    return dsl.selectFrom(FACILITY)
              .where(getConditions(param))
              .orderBy(FACILITY.CRE_DTM.desc())
              .limit(param.getSize())
              .offset(param.getOffset())
              .fetch()
              .map(FacilityListResponse::new);
  }

  /**
   * 시설물 목록 조회 조건 목록
   */
  public List<Condition> getConditions(FacilityListRequest param) {
    List<Condition> conditions = new ArrayList<>();

    // 시설물 일련번호
    if (param.getFacilitySeq() != null) {
      conditions.add(DSL.condition(FACILITY.FCLTY_SEQ.eq(ULong.valueOf(param.getFacilitySeq()))));
    }
    // 시설물 종류
    if (StringUtils.isNotBlank(param.getType())) {
      conditions.add(DSL.condition(FACILITY.FCLTY_TYP.eq(param.getType())));
    }
    // 위치
    if (StringUtils.isNotBlank(param.getLocation())) {
      conditions.add(DSL.condition(FACILITY.FCLTY_DTL_LCTN.like("%" + param.getLocation() + "%")));
    }
    // 승인 상태
    if (StringUtils.isNotBlank(param.getApprovalStatus())) {
      conditions.add(DSL.condition(FACILITY.FCLTY_APRV_STA.eq(param.getApprovalStatus())));
    }
    // 생성일 검색 시작일
    if (StringUtils.isNotBlank(param.getStartDate())) {
      LocalDateTime startDate = DateTimeUtils.convertDateStringToDateTime(param.getStartDate());
      conditions.add(DSL.condition(FACILITY.CRE_DTM.ge(startDate)));
    }
    // 생성일 검색 종료일
    if (StringUtils.isNotBlank(param.getEndDate())) {
      LocalDateTime endDate = DateTimeUtils.convertToDate(param.getEndDate()).atTime(LocalTime.MAX);
      conditions.add(DSL.condition(FACILITY.CRE_DTM.le(endDate)));
    }

    return conditions;
  }
}
