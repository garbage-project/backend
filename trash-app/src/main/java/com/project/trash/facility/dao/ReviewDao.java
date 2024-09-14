package com.project.trash.facility.dao;

import com.project.trash.facility.domain.enums.FacilityApprovalStatus;
import com.project.trash.facility.request.FacilityListRequest;
import com.project.trash.facility.response.FacilityListResponse;
import com.project.trash.member.response.MyFacilityListResponse;
import com.project.trash.member.response.MyReviewListResponse;
import com.project.trash.utils.MemberUtils;

import org.jooq.Condition;
import org.jooq.DSLContext;
import org.jooq.impl.DSL;
import org.jooq.types.ULong;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.List;

import lombok.RequiredArgsConstructor;

import static trash.tables.Facility.FACILITY;
import static trash.tables.Review.REVIEW;

/**
 * 리뷰 조회 DAO
 */
@Repository
@RequiredArgsConstructor
public class ReviewDao {

  private final DSLContext dsl;

  /**
   * 등록한 리뷰 목록 조회
   */
  public List<MyReviewListResponse> select() {
    return dsl.select(REVIEW.RVW_ID, FACILITY.FCLTY_NM, REVIEW.RVW_CTT, REVIEW.CRE_DTM)
        .from(REVIEW)
        .leftJoin(FACILITY)
        .on(FACILITY.FCLTY_ID.eq(REVIEW.FCLTY_ID))
        .where(REVIEW.MBR_ID.eq(ULong.valueOf(MemberUtils.getMemberId())))
        .orderBy(REVIEW.CRE_DTM.desc())
        .fetchInto(MyReviewListResponse.class);
  }
}
