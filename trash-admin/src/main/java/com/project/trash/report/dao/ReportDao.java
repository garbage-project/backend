package com.project.trash.report.dao;

import com.project.trash.common.domain.enums.Valid;
import com.project.trash.common.utils.DateTimeUtils;
import com.project.trash.report.request.ReportListRequest;
import com.project.trash.report.response.ReportListResponse;

import org.apache.commons.lang3.StringUtils;
import org.jooq.Condition;
import org.jooq.DSLContext;
import org.jooq.Record2;
import org.jooq.impl.DSL;
import org.jooq.types.ULong;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.List;

import lombok.RequiredArgsConstructor;
import trash.tables.records.ReportRecord;

import static org.jooq.impl.DSL.when;
import static trash.Tables.MEMBER;
import static trash.Tables.REPORT;

/**
 * 신고 조회 DAO
 */
@Repository
@RequiredArgsConstructor
public class ReportDao {

  private final DSLContext dsl;

  /**
   * 신고 목록 조회 총개수
   */
  public Long count(ReportListRequest param) {
    return dsl.selectCount().from(REPORT).where(getConditions(param)).fetchOneInto(Long.class);
  }

  /**
   * 신고 목록 조회 조건 목록
   */
  public List<Condition> getConditions(ReportListRequest param) {
    List<Condition> conditions = new ArrayList<>();

    // 신고 내용
    if (StringUtils.isNotBlank(param.getContent())) {
      conditions.add(DSL.condition(REPORT.RPT_CTT.like("%" + param.getContent() + "%")));
    }
    // 상태
    if (StringUtils.isNotBlank(param.getStatus())) {
      if (param.getStatus().equals("Y")) {
        conditions.add(DSL.condition(REPORT.RPT_ANS.isNotNull()));
      } else {
        conditions.add(DSL.condition(REPORT.RPT_ANS.isNull()));
      }
    }
    // 회원 일련번호(신고자 ID)
    if (param.getMemberSeq() != null) {
      conditions.add(DSL.condition(REPORT.MBR_SEQ.eq(ULong.valueOf(param.getMemberSeq()))));
    }
    // 시설물 ID
    if (StringUtils.isNotBlank(param.getFacilityId())) {
      conditions.add(DSL.condition(REPORT.FCLTY_ID.eq(param.getFacilityId())));
    }
    // 생성일 검색 시작일
    if (StringUtils.isNotBlank(param.getStartDate())) {
      LocalDateTime startDate = DateTimeUtils.convertDateStringToDateTime(param.getStartDate());
      conditions.add(DSL.condition(REPORT.CRE_DTM.ge(startDate)));
    }
    // 생성일 검색 종료일
    if (StringUtils.isNotBlank(param.getEndDate())) {
      LocalDateTime endDate = DateTimeUtils.convertToDate(param.getEndDate()).atTime(LocalTime.MAX);
      conditions.add(DSL.condition(REPORT.CRE_DTM.le(endDate)));
    }
    return conditions;
  }

  /**
   * 신고 상세 조회
   */
  public Record2<ReportRecord, String> select(Long reportSeq) {
    return dsl.select(REPORT, MEMBER.MBR_NCK_NM)
              .from(REPORT)
              .leftJoin(MEMBER)
              .on(MEMBER.MBR_SEQ.eq(REPORT.MBR_SEQ))
              .where(REPORT.RPT_SEQ.eq(ULong.valueOf(reportSeq)))
              .fetchOne();
  }

  /**
   * 신고 목록 조회
   */
  public List<ReportListResponse> select(ReportListRequest param) {
    return dsl.select(REPORT.RPT_SEQ, REPORT.RPT_CTT,
                  when(REPORT.RPT_ANS.isNull(), Valid.FALSE.getCode()).otherwise(Valid.TRUE.getCode()), REPORT.MBR_SEQ,
                  REPORT.FCLTY_ID, REPORT.CRE_DTM)
              .from(REPORT)
              .where(getConditions(param))
              .orderBy(REPORT.CRE_DTM.desc())
              .limit(param.getSize())
              .offset(param.getOffset())
              .fetchInto(ReportListResponse.class);
  }
}
