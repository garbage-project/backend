package com.project.trash.common.utils;

import org.apache.commons.lang3.StringUtils;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.TextStyle;
import java.util.Locale;

import lombok.experimental.UtilityClass;

/**
 * 날짜/시간 유틸
 */
@UtilityClass
public class DateTimeUtils {

  /**
   * 기본 Date 형식 (yyyy.MM.dd)
   */
  public final String DEFAULT_DATE = "yyyy-MM-dd";
  /**
   * 기본 년월일 형식 (yyyyMMdd)
   */
  public final String DEFAULT_DAY = "yyyyMMdd";
  /**
   * 기본 DateTime 형식 (yyyy-MM-dd HH:mm:ss)
   */
  public final String DEFAULT_DATETIME = "yyyy-MM-dd HH:mm:ss";
  /**
   * 조회 기본 DateTime 형식 (yyyy-MM-dd HH:mm)
   */
  public final String DISPLAY_DEFAULT_DATETIME = "yyyy-MM-dd HH:mm";

  /**
   * LocalDate 문자열 -> 다른 패턴의 LocalDate 문자열
   */
  public String convertDateStringToDateString(String dateString, String fromPattern, String toPattern) {
    if (StringUtils.isBlank(dateString)) {
      return null;
    }
    LocalDate date = LocalDate.parse(dateString, DateTimeFormatter.ofPattern(fromPattern));
    return date.format(DateTimeFormatter.ofPattern(toPattern));
  }

  /**
   * LocalDate 문자열 -> LocalDateTime
   */
  public LocalDateTime convertDateStringToDateTime(String dateString) {
    if (StringUtils.isBlank(dateString)) {
      return null;
    }
    DateTimeFormatter formatter = DateTimeFormatter.ofPattern(DEFAULT_DATE);
    return LocalDate.parse(dateString, formatter).atStartOfDay();
  }

  /**
   * yyyy-MM-dd 문자열 -> LocalDate
   */
  public LocalDate convertToDate(String dateString) {
    DateTimeFormatter formatter = DateTimeFormatter.ofPattern(DEFAULT_DATE, Locale.KOREA);
    return LocalDate.parse(dateString, formatter);
  }

  /**
   * LocalDate 문자열 -> LocalDate
   */
  public LocalDate convertToDate(String dateString, String pattern) {
    DateTimeFormatter formatter = DateTimeFormatter.ofPattern(pattern, Locale.KOREA);
    return LocalDate.parse(dateString, formatter);
  }

  /**
   * yyyy-MM-dd HH:mm:ss 문자열 -> LocalDateTime
   */
  public LocalDateTime convertToDateTime(String dateTimeString) {
    DateTimeFormatter formatter = DateTimeFormatter.ofPattern(DEFAULT_DATETIME, Locale.KOREA);
    return LocalDateTime.parse(dateTimeString, formatter);
  }

  /**
   * LocalDateTime -> yyyy-MM-dd HH:mm:ss 문자열 변환
   */
  public String convertToString(LocalDateTime dateTime, String pattern) {
    DateTimeFormatter formatter = DateTimeFormatter.ofPattern(pattern, Locale.KOREA);
    return dateTime.format(formatter);
  }

  /**
   * LocalDate -> yyyy-MM-dd 문자열 변환
   */
  public String convertToString(LocalDate date, String pattern) {
    DateTimeFormatter formatter = DateTimeFormatter.ofPattern(pattern, Locale.KOREA);
    return date.format(formatter);
  }

  /**
   * LocalDateTime -> 24. 6. 17. (월) 23:51 형식의 문자열 반환
   */
  public String formatLocalDateTime(LocalDateTime dateTime) {
    // 요일
    String dayOfWeek = dateTime.getDayOfWeek().getDisplayName(TextStyle.SHORT, Locale.KOREAN);

    // 날짜와 시간을 포맷팅 (yyyy, MM, dd, HH, mm을 필요에 맞게 포맷팅)
    DateTimeFormatter dateFormatter = DateTimeFormatter.ofPattern("yy. M. d. ");
    DateTimeFormatter timeFormatter = DateTimeFormatter.ofPattern("HH:mm");

    // 포맷팅된 날짜와 시간
    String formattedDate = dateTime.format(dateFormatter);
    String formattedTime = dateTime.format(timeFormatter);

    // 최종 형식으로 결합
    return formattedDate + "(" + dayOfWeek + ") " + formattedTime;
  }

  /**
   * 시작날짜가 종료날짜보다 앞 날짜인지 확인
   *
   * @param start 시작날짜
   * @param end   종료날짜
   * @return 시작일시 유효여부
   */
  public boolean isBeforeDate(String start, String end) {
    if (StringUtils.isBlank(start) || StringUtils.isBlank(end)) {
      return true;
    }
    DateTimeFormatter formatter = DateTimeFormatter.ofPattern(DEFAULT_DATE);
    LocalDate startDate = LocalDate.parse(start, formatter);
    LocalDate endDate = LocalDate.parse(end, formatter);

    return startDate.isEqual(endDate) || startDate.isBefore(endDate);
  }

  public boolean validFormat(String dateString) {
    return validFormat(dateString, DEFAULT_DATE);
  }

  /**
   * 날짜 문자열 포맷 유효성 체크
   *
   * @param dateString 날짜 문자열
   * @param pattern    패턴
   * @return 유효여부
   */
  public boolean validFormat(String dateString, String pattern) {
    if (StringUtils.isBlank(dateString)) {
      return false;
    }
    DateTimeFormatter formatter = DateTimeFormatter.ofPattern(pattern, Locale.KOREA);
    try {
      if (StringUtils.containsAny(pattern, "H", "m", "s", "S")) {
        LocalDateTime.parse(dateString, formatter);
      } else {
        LocalDate.parse(dateString, formatter);
      }
      return true;
    } catch (Exception e) {
      return false;
    }
  }
}
