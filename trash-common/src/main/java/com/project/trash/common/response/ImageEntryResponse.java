package com.project.trash.common.response;

import java.util.List;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;

@Getter
@Schema(title = "이미지 등록 결과 응답")
public class ImageEntryResponse {

  private List<Long> imageIds;

  public ImageEntryResponse(List<Long> imageIds) {
    this.imageIds = imageIds;
  }
}
