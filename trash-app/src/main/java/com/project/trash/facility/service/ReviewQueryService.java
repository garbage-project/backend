package com.project.trash.facility.service;

import com.project.trash.common.exception.ValidationException;
import com.project.trash.facility.dao.ReviewDao;
import com.project.trash.facility.domain.Facility;
import com.project.trash.facility.domain.Review;
import com.project.trash.facility.repository.FacilityRepository;
import com.project.trash.facility.repository.ReviewRepository;
import com.project.trash.facility.request.FacilityReviewListRequest;
import com.project.trash.facility.response.FacilityReviewListResponse;
import com.project.trash.member.response.MyReviewListResponse;
import com.project.trash.utils.MemberUtils;

import org.apache.commons.lang3.tuple.Pair;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import lombok.RequiredArgsConstructor;

import static com.project.trash.common.domain.resultcode.ReviewResultCode.REVIEW_NOT_FOUND;

/**
 * 리뷰 조회 서비스
 */
@Service
@RequiredArgsConstructor
public class ReviewQueryService {

  private final ReviewRepository reviewRepository;
  private final ReviewDao reviewDao;

  /**
   * 로그인 회원이 등록한 리뷰 목록 조회
   */
  public List<MyReviewListResponse> getList() {
    return reviewDao.select();
  }

  /**
   * 시설물 리뷰 목록 조회
   */
  @Transactional(readOnly = true)
  public Pair<List<FacilityReviewListResponse>, Long> getList(FacilityReviewListRequest param) {
    return Pair.of(reviewDao.select(param), reviewDao.count(param));
  }

  /**
   * 리뷰 조회
   */
  @Transactional(readOnly = true)
  public Review getOne(Long reviewSeq, Long memberSeq) {
    return reviewRepository.findByReviewSeqAndMemberSeq(reviewSeq, memberSeq)
                           .orElseThrow(() -> new ValidationException(REVIEW_NOT_FOUND));
  }
}
