package com.project.trash.review.service;

import com.project.trash.common.exception.ValidationException;
import com.project.trash.review.domain.Review;
import com.project.trash.review.repository.ReviewRepository;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;

/**
 * 리뷰 조회 서비스
 */
@Service
@RequiredArgsConstructor
public class ReviewQueryService {

  private final ReviewRepository reviewRepository;

  /**
   * 리뷰 조회
   */
  @Transactional(readOnly = true)
  public Review getOne(Long reviewSeq, Long memberSeq) {
    return reviewRepository.findByReviewSeqAndMemberSeq(reviewSeq, memberSeq)
                           .orElseThrow(() -> new ValidationException("review.not_found"));
  }
}
