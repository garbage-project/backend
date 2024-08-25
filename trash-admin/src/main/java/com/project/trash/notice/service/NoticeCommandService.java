package com.project.trash.notice.service;

import com.project.trash.common.domain.enums.Valid;
import com.project.trash.notice.domain.Notice;
import com.project.trash.notice.repository.NoticeRepository;
import com.project.trash.notice.request.NoticeEntryRequest;
import com.project.trash.notice.request.NoticeModifyRequest;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;

/**
 * 공지 등록/수정/삭제 서비스
 */
@Service
@RequiredArgsConstructor
public class NoticeCommandService {

  private final NoticeRepository noticeRepository;
  private final NoticeQueryService noticeQueryService;

  /**
   * 공지 삭제
   */
  @Transactional
  public void delete(Long reviewSeq) {
    Notice notice = noticeQueryService.getOne(reviewSeq);

    noticeRepository.delete(notice);
  }

  /**
   * 공지 등록
   */
  @Transactional
  public void entry(NoticeEntryRequest param) {
    noticeRepository.save(new Notice(param.getTitle(), param.getContent(), Valid.convertToBoolean(param.getValid())));
  }

  /**
   * 공지 수정
   */
  @Transactional
  public void modify(NoticeModifyRequest param) {
    Notice notice = noticeQueryService.getOne(param.getNoticeSeq());

    notice.update(param.getTitle(), param.getContent(), Valid.convertToBoolean(param.getValid()));
  }
}
