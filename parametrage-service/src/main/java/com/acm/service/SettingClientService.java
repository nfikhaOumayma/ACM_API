/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import org.springframework.web.multipart.MultipartFile;

import com.acm.exceptions.type.ResourcesNotFoundException;

/**
 * {@link SettingClientService} class.
 *
 * @author Ines Dridi
 * @since 1.0.1
 */
public interface SettingClientService {

	/**
	 * Save.
	 *
	 * @author Ines Dridi
	 * @param uploadedFiles the uploaded files
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	void save(MultipartFile[] uploadedFiles) throws ResourcesNotFoundException;
}
