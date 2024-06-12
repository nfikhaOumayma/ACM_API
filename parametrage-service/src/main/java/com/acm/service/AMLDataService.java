/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import org.springframework.web.multipart.MultipartFile;

import com.acm.utils.dtos.AMLDataDTO;

/**
 * {@link AMLDataService} interface.
 *
 * @author HaythemBenizid
 * @since 1.0.0
 */
public interface AMLDataService {

	/**
	 * Find {@link List} of {@link AMLDataDTO} by given params.
	 *
	 * @author HaythemBenizid
	 * @param aMLDataDTO the aMLData DTO
	 * @return the list
	 */
	List<AMLDataDTO> find(AMLDataDTO aMLDataDTO);

	/**
	 * check AML {@link List} of {@link AMLDataDTO} by given by NAME / IDENTITY.
	 *
	 * @author HaythemBenizid
	 * @param aMLDataDTO the aMLData DTO
	 * @return the list
	 */
	List<AMLDataDTO> checkAML(AMLDataDTO aMLDataDTO);

	/**
	 * The method used for saving the given {@link AMLDataDTO}.
	 *
	 * @author HaythemBenizid
	 * @param aMLDataDTO the aMLData DTO
	 * @return the aMLData DTO
	 */
	AMLDataDTO save(AMLDataDTO aMLDataDTO);

	/**
	 * Upload aml file.
	 *
	 * @author ManelLamloum
	 * @param uploadedFiles the uploaded files
	 */
	void uploadAmlFile(MultipartFile[] uploadedFiles);
}
