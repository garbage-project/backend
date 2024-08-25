package com.project.trash.notice.repository;

import com.project.trash.notice.domain.Notice;

import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface NoticeRepository extends JpaRepository<Notice, Long> {

  List<Notice> findAllByValid(Boolean valid);
}
