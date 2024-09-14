package com.project.trash.notice.service;

import com.project.trash.common.exception.ValidationException;
import com.project.trash.notice.dao.NoticeDao;
import com.project.trash.notice.domain.Notice;
import com.project.trash.notice.repository.NoticeRepository;
import com.project.trash.notice.request.NoticeListRequest;
import com.project.trash.notice.response.NoticeDetailResponse;
import com.project.trash.notice.response.NoticeListResponse;

import org.apache.commons.lang3.tuple.Pair;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

import lombok.RequiredArgsConstructor;

import static com.project.trash.common.domain.resultcode.NoticeResultCode.NOTICE_NOT_FOUND;

/**
 * 공지 조회 서비스
 */
@Service
@RequiredArgsConstructor
public class NoticeQueryService {

  private final NoticeRepository noticeRepository;
  private final NoticeDao noticeDao;

  /**
   * 공지 상세 조회
   */
  @Transactional(readOnly = true)
  public NoticeDetailResponse getDetail(Long noticeId) {
    return new NoticeDetailResponse(getOne(noticeId));
  }

  /**
   * 공지 목록 조회
   */
  @Transactional(readOnly = true)
  public Pair<List<NoticeListResponse>, Long> getList(NoticeListRequest param) {
    return Pair.of(noticeDao.select(param), noticeDao.count(param));
  }

  /**
   * 공지 조회
   */
  @Transactional(readOnly = true)
  public Notice getOne(Long noticeId) {
    return noticeRepository.findById(noticeId).orElseThrow(() -> new ValidationException(NOTICE_NOT_FOUND));
  }
}
