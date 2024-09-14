package com.project.trash.notice.dao;

import com.project.trash.common.utils.DateTimeUtils;
import com.project.trash.notice.request.NoticeListRequest;
import com.project.trash.notice.response.NoticeListResponse;

import org.apache.commons.lang3.StringUtils;
import org.jooq.Condition;
import org.jooq.DSLContext;
import org.jooq.impl.DSL;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.List;

import lombok.RequiredArgsConstructor;

import static trash.Tables.NOTICE;

/**
 * 공지 조회 DAO
 */
@Repository
@RequiredArgsConstructor
public class NoticeDao {

  private final DSLContext dsl;

  /**
   * 공지 목록 조회 총개수
   */
  public Long count(NoticeListRequest param) {
    return dsl.selectCount().from(NOTICE).where(getConditions(param)).fetchOneInto(Long.class);
  }

  /**
   * 공지 목록 조회 조건 목록
   */
  public List<Condition> getConditions(NoticeListRequest param) {
    List<Condition> conditions = new ArrayList<>();

    // 제목
    if (StringUtils.isNotBlank(param.getTitle())) {
      conditions.add(DSL.condition(NOTICE.NTC_TTL.like("%" + param.getTitle() + "%")));
    }
    // 유효여부
    if (StringUtils.isNotBlank(param.getValid())) {
      conditions.add(DSL.condition(NOTICE.NTC_VLD_YN.eq(param.getValid())));
    }
    // 생성일 검색 시작일
    if (StringUtils.isNotBlank(param.getStartDate())) {
      LocalDateTime startDate = DateTimeUtils.convertDateStringToDateTime(param.getStartDate());
      conditions.add(DSL.condition(NOTICE.CRE_DTM.ge(startDate)));
    }
    // 생성일 검색 종료일
    if (StringUtils.isNotBlank(param.getEndDate())) {
      LocalDateTime endDate = DateTimeUtils.convertToDate(param.getEndDate()).atTime(LocalTime.MAX);
      conditions.add(DSL.condition(NOTICE.CRE_DTM.le(endDate)));
    }
    return conditions;
  }

  /**
   * 공지 목록 조회
   */
  public List<NoticeListResponse> select(NoticeListRequest param) {
    return dsl.select(NOTICE.NTC_ID, NOTICE.NTC_TTL, NOTICE.NTC_CTT, NOTICE.NTC_VLD_YN, NOTICE.CRE_DTM)
              .from(NOTICE)
              .where(getConditions(param))
              .orderBy(NOTICE.CRE_DTM.desc())
              .limit(param.getSize())
              .offset(param.getOffset())
              .fetchInto(NoticeListResponse.class);
  }
}
