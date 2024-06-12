/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.acm.service.AMLDataService;
import com.acm.utils.dtos.AMLDataDTO;

/**
 * This class @{link AMLDataController} used to control all the {@link AMLDataDTO} requests.
 *
 * @author HaythemBenizid
 * @since 1.0.0
 */
@RestController
@RequestMapping("/aml-settings")
public class AMLDataController {

	/** The AMLData service. */
	@Autowired
	private AMLDataService aMLDataService;

	/**
	 * Find {@link List} of {@link AMLDataDTO} by Requested params.
	 *
	 * @author HaythemBenizid
	 * @param aMLDataDTO the aMLData DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<AMLDataDTO> find(@RequestBody AMLDataDTO aMLDataDTO) {

		return aMLDataService.find(aMLDataDTO);
	}

	/**
	 * check AML {@link List} of {@link AMLDataDTO} by Name.
	 *
	 * @author HaythemBenizid
	 * @param aMLDataDTO the aMLData DTO
	 * @return the list
	 */
	@PostMapping("/check-aml")
	public List<AMLDataDTO> checkAML(@RequestBody AMLDataDTO aMLDataDTO) {

		return aMLDataService.checkAML(aMLDataDTO);
	}

	/**
	 * Upload aml file.
	 *
	 * @author ManelLamloum
	 * @param uploadedFiles the uploaded files
	 */
	@PostMapping("/upload-aml-file")
	public void uploadAmlFile(@RequestParam("uploadedFiles") MultipartFile[] uploadedFiles) {

		aMLDataService.uploadAmlFile(uploadedFiles);
	}
}
