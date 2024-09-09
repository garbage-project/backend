package com.project.trash.review.dao;

import com.project.trash.facility.request.FacilityReviewListRequest;
import com.project.trash.facility.response.FacilityReviewListResponse;
import com.project.trash.member.request.MemberReviewListRequest;
import com.project.trash.member.response.MemberReviewListResponse;

import org.jooq.DSLContext;
import org.jooq.types.ULong;
import org.springframework.stereotype.Repository;

import java.util.List;

import lombok.RequiredArgsConstructor;

import static trash.Tables.FACILITY;
import static trash.Tables.MEMBER;
import static trash.Tables.REVIEW;

/**
 * 리뷰 조회 DAO
 */
@Repository
@RequiredArgsConstructor
public class ReviewDao {

  private final DSLContext dsl;

  /**
   * 회원이 등록한 리뷰 목록 총개수
   */
  public Long count(Long memberSeq) {
    return dsl.selectCount()
              .from(REVIEW)
              .where(REVIEW.MBR_SEQ.eq(ULong.valueOf(memberSeq)))
              .fetchOneInto(Long.class);
  }

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
   * 회원이 등록한 리뷰 목록 조회
   */
  public List<MemberReviewListResponse> select(MemberReviewListRequest param) {
    return dsl.select(REVIEW)
        .from(REVIEW)
        .leftJoin(FACILITY)
        .on(FACILITY.FCLTY_SEQ.eq(REVIEW.FCLTY_SEQ))
        .where(REVIEW.MBR_SEQ.eq(ULong.valueOf(param.getMemberSeq())))
        .orderBy(REVIEW.CRE_DTM.desc())
        .limit(param.getSize())
        .offset(param.getOffset())
        .fetchInto(MemberReviewListResponse.class);
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
