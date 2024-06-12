/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.io.IOException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.SettingClientService;

/**
 * {@link SettingClientController} SettingClientController class.
 *
 * @author Ines Dridi
 * @since 1.0.1
 */
@RestController
@RequestMapping("/setting-client")
public class SettingClientController {

	/** The AcmDocuments service. */
	@Autowired
	private SettingClientService settingClientService;

	/**
	 * Save image to ged.
	 * 
	 * @author Ines Dridi
	 * @param uploadedFiles the uploaded files
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/save-client-image-to-ged")
	public void saveImageToGed(@RequestParam("uploadedFiles") MultipartFile[] uploadedFiles)
			throws IOException, ResourcesNotFoundException {

		settingClientService.save(uploadedFiles);
	}
}
