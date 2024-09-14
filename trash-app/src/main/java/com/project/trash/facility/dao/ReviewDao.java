package com.project.trash.facility.dao;

import com.project.trash.facility.domain.enums.FacilityApprovalStatus;
import com.project.trash.facility.request.FacilityListRequest;
import com.project.trash.facility.request.FacilityReviewListRequest;
import com.project.trash.facility.response.FacilityListResponse;
import com.project.trash.facility.response.FacilityReviewListResponse;
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
import trash.Tables;

import static trash.Tables.MEMBER;
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
   * 시설물 리뷰 목록 총개수
   */
  public Long count(FacilityReviewListRequest param) {
    return dsl.selectCount()
              .from(REVIEW)
              .where(REVIEW.FCLTY_SEQ.eq(ULong.valueOf(param.getFacilitySeq())))
              .fetchOneInto(Long.class);
  }

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

  /**
   * 시설물 리뷰 목록 조회
   */
  public List<FacilityReviewListResponse> select(FacilityReviewListRequest param) {
    return dsl.select(REVIEW.RVW_SEQ, REVIEW.RVW_CTT, REVIEW.CRE_DTM, MEMBER.MBR_SEQ, MEMBER.MBR_NCK_NM)
              .from(REVIEW)
              .leftJoin(MEMBER)
              .on(MEMBER.MBR_SEQ.eq(REVIEW.MBR_SEQ))
              .where(REVIEW.FCLTY_SEQ.eq(ULong.valueOf(param.getFacilitySeq())))
              .orderBy(REVIEW.CRE_DTM.desc())
              .limit(param.getSize())
              .offset(param.getOffset())
              .fetchInto(FacilityReviewListResponse.class);
  }
}
