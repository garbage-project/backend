package com.project.trash.review.dao;

import com.project.trash.member.request.MemberReviewListRequest;

import org.jooq.DSLContext;
import org.jooq.types.ULong;
import org.springframework.stereotype.Repository;

import java.util.List;

import lombok.RequiredArgsConstructor;
import trash.tables.records.ReviewRecord;

import static trash.Tables.REVIEW;

/**
 * 리뷰 조회 DAO
 */
@Repository
@RequiredArgsConstructor
public class ReviewDao {

  private final DSLContext dsl;

  /**
   * 등록한 리뷰 목록 조회 총개수
   */
  public Long count(Long memberSeq) {
    return dsl.selectCount().from(REVIEW).where(REVIEW.MBR_SEQ.eq(ULong.valueOf(memberSeq))).fetchOneInto(Long.class);
  }

  /**
   * 등록한 리뷰 목록 조회
   */
  public List<ReviewRecord> select(MemberReviewListRequest param) {
    return dsl.selectFrom(REVIEW)
              .where(REVIEW.MBR_SEQ.eq(ULong.valueOf(param.getMemberSeq())))
              .orderBy(REVIEW.CRE_DTM.desc())
              .limit(param.getSize())
              .offset(param.getOffset())
              .fetch();
  }
}
