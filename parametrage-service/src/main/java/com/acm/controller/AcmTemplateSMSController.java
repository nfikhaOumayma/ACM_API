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
import org.springframework.web.bind.annotation.RestController;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.AcmTemplateSMSService;
import com.acm.utils.dtos.AcmTemplateSMSDTO;

/**
 * The Class AcmTemplateSMSController.
 */
@RestController
@RequestMapping("/settings-sms")
public class AcmTemplateSMSController {

	/** The acm template SMS service. */
	@Autowired
	private AcmTemplateSMSService acmTemplateSMSService;

	/**
	 * Find all.
	 *
	 * @param acmTemplateSMSDTO the acm template SMSDTO
	 * @return the list
	 */
	@PostMapping("/find")
	public List<AcmTemplateSMSDTO> findAll(@RequestBody AcmTemplateSMSDTO acmTemplateSMSDTO) {

		return acmTemplateSMSService.find(acmTemplateSMSDTO);
	}

	/**
	 * Findbycode.
	 *
	 * @param acmTemplateSMSDTO the acm template SMSDTO
	 * @return the acm template SMSDTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/findTemplate")
	public AcmTemplateSMSDTO findbycode(@RequestBody AcmTemplateSMSDTO acmTemplateSMSDTO)
			throws ResourcesNotFoundException {

		return acmTemplateSMSService.findByCode(acmTemplateSMSDTO);
	}

	/**
	 * Save.
	 *
	 * @param acmTemplateSMSDTO the acm template SMSDTO
	 * @return the acm template SMSDTO
	 */
	@PostMapping("/save")
	public AcmTemplateSMSDTO save(@RequestBody AcmTemplateSMSDTO acmTemplateSMSDTO) {

		return acmTemplateSMSService.save(acmTemplateSMSDTO);
	}

	/**
	 * Update.
	 *
	 * @param acmTemplateSMSDTO the acm template SMSDTO
	 * @return the acm template SMSDTO
	 */
	@PostMapping("/update")
	public AcmTemplateSMSDTO update(@RequestBody AcmTemplateSMSDTO acmTemplateSMSDTO) {

		return acmTemplateSMSService.updateMessageBody(acmTemplateSMSDTO);
	}
}
