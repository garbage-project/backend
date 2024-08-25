package com.project.trash.review.service;

import com.project.trash.facility.domain.Facility;
import com.project.trash.facility.repository.FacilityRepository;
import com.project.trash.facility.repository.ReviewRepository;
import com.project.trash.member.request.MemberReviewListRequest;
import com.project.trash.member.response.MemberReviewListResponse;
import com.project.trash.review.dao.ReviewDao;

import org.apache.commons.lang3.tuple.Pair;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import lombok.RequiredArgsConstructor;
import trash.tables.records.ReviewRecord;

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
   * 리뷰 목록 조회
   */
  @Transactional(readOnly = true)
  public Pair<List<MemberReviewListResponse>, Long> getList(MemberReviewListRequest param) {
    // 1. 리뷰 목록 조회
    List<ReviewRecord> reviews = reviewDao.select(param);

    // 2. 시설물 ID 목록 추출
    List<String> facilityIds = reviews.stream().map(ReviewRecord::getFcltyId).distinct().toList();

    // 3. 시설물 조회
    List<Facility> facilities = facilityRepository.findAllById(facilityIds);

    // 4. 시설물 ID와 시설물명 매핑
    Map<String, Facility> facilityMap =
        facilities.stream().collect(Collectors.toMap(Facility::getFacilityId, facility -> facility));

    List<MemberReviewListResponse> list = reviews.stream()
                                                 .map(review -> new MemberReviewListResponse(review,
                                                     facilityMap.get(review.getFcltyId())))
                                                 .toList();
    
    return Pair.of(list, reviewDao.count(param.getMemberSeq()));
  }
}
