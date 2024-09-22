package com.project.trash.facility.service;

import com.project.trash.common.exception.ValidationException;
import com.project.trash.common.request.PageRequest;
import com.project.trash.facility.dao.ReviewDao;
import com.project.trash.facility.domain.Review;
import com.project.trash.facility.repository.FacilityRepository;
import com.project.trash.facility.repository.ReviewRepository;
import com.project.trash.facility.response.FacilityReviewListResponse;
import com.project.trash.member.response.MyReviewListResponse;

import org.apache.commons.lang3.tuple.Pair;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

import lombok.RequiredArgsConstructor;

import static com.project.trash.common.domain.resultcode.FacilityResultCode.FACILITY_NOT_FOUND;
import static com.project.trash.common.domain.resultcode.ReviewResultCode.REVIEW_NOT_FOUND;

/**
 * 리뷰 조회 서비스
 */
@Service
@RequiredArgsConstructor
public class ReviewQueryService {

  private final ReviewRepository reviewRepository;
  private final ReviewDao reviewDao;

  private final FacilityRepository facilityRepository;

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
  public Pair<List<FacilityReviewListResponse>, Long> getList(Long facilityId, PageRequest param) {
    verifyFacilityExist(facilityId);

    return Pair.of(reviewDao.select(facilityId, param), reviewDao.count(facilityId));
  }

  /**
   * 리뷰 조회
   */
  @Transactional(readOnly = true)
  public Review getOne(Long reviewId, Long memberId) {
    return reviewRepository.findByReviewIdAndMemberId(reviewId, memberId)
                           .orElseThrow(() -> new ValidationException(REVIEW_NOT_FOUND));
  }

  /**
   * 시설물 리뷰 개수 조회
   */
  public Long getCount(Long facilityId) {
    verifyFacilityExist(facilityId);

    return reviewDao.count(facilityId);
  }

  /**
   * 시설물 존재여부 검증
   */
  private void verifyFacilityExist(Long facilityId) {
    if (!facilityRepository.existsById(facilityId)) {
      throw new ValidationException(FACILITY_NOT_FOUND);
    }
  }
}
