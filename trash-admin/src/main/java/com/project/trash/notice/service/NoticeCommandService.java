package com.project.trash.notice.service;

import com.project.trash.common.domain.enums.Valid;
import com.project.trash.notice.domain.Notice;
import com.project.trash.notice.repository.NoticeRepository;
import com.project.trash.notice.request.NoticeEntryRequest;
import com.project.trash.notice.request.NoticeModifyRequest;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
public class NoticeCommandService {

  private final NoticeRepository noticeRepository;
  private final NoticeQueryService noticeQueryService;

  @Transactional
  public void delete(Long reviewId) {
    Notice notice = noticeQueryService.getOne(reviewId);

    noticeRepository.delete(notice);
  }

  @Transactional
  public void entry(NoticeEntryRequest param) {
    noticeRepository.save(new Notice(param.getTitle(), param.getContent(), Valid.convertToBoolean(param.getValid())));
  }

  @Transactional
  public void modify(NoticeModifyRequest param) {
    Notice notice = noticeQueryService.getOne(param.getNoticeId());

    notice.update(param.getTitle(), param.getContent(), Valid.convertToBoolean(param.getValid()));
  }
}
