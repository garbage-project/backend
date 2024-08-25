package com.project.trash.notice.service;

import com.project.trash.notice.repository.NoticeRepository;
import com.project.trash.notice.response.NoticeListResponse;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Comparator;
import java.util.List;

import lombok.RequiredArgsConstructor;

/**
 * 공지 조회 서비스
 */
@Service
@RequiredArgsConstructor
public class NoticeQueryService {

  private final NoticeRepository noticeRepository;

  /**
   * 공지 목록 조회
   */
  @Transactional(readOnly = true)
  public List<NoticeListResponse> getList() {
    return noticeRepository.findAllByValid(Boolean.TRUE)
                           .stream()
                           .map(NoticeListResponse::new)
                           .sorted(Comparator.comparing(NoticeListResponse::getCreatedAt).reversed())
                           .toList();
  }
}
