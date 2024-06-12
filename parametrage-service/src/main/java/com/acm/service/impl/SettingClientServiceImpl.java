/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.acm.client.GedClient;
import com.acm.constants.common.CommonConstantGED;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.AcmEnvironnementService;
import com.acm.service.SettingClientService;
import com.acm.utils.dtos.AcmEnvironnementDTO;
import com.acm.utils.dtos.GedDocumentDTO;
import com.acm.utils.dtos.GedParameterDTO;
import com.acm.utils.enums.SettingCategory;
import com.acm.utils.validation.ACMValidationUtils;

/**
 * {@link SettingClientServiceImpl} class.
 *
 * @author Ines Dridi
 * @since 1.0.1
 */
@Service
public class SettingClientServiceImpl implements SettingClientService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(SettingClientServiceImpl.class);

	/** The acmEnvironnement service. */
	@Autowired
	private AcmEnvironnementService acmEnvironnementService;

	/** The environment. */
	@Autowired
	private Environment environment;

	/** The ged client. */
	@Autowired
	private GedClient gedClient;

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.SettingClientService#save(org.springframework.web.multipart.MultipartFile[])
	 */
	@Override
	public void save(MultipartFile[] uploadedFiles) throws ResourcesNotFoundException {

		// check if the param is not null
		if (uploadedFiles != null && uploadedFiles.length > 0) {
			// init data to be send to GED
			GedParameterDTO gedParameterDTO = new GedParameterDTO();
			gedParameterDTO.setPath(CommonConstants.APP_CLIENT);
			gedParameterDTO.setSite(CommonConstantGED.ACM_SITE);
			// setting tags
			List<String> tags = new ArrayList<>();
			String photoTag =
					CommonConstants.APP_CLIENT + CommonConstantGED.TIR_6 + CommonConstantGED.LOGO;
			tags.add(photoTag);
			gedParameterDTO.setTags(tags);
			// convert MultipartFile to File
			List<File> filesToSend = new ArrayList<>();
			for (MultipartFile multipartFile : uploadedFiles) {
				File file = CommonFunctions.fileConverter(multipartFile,
						environment.getProperty("spring.servlet.multipart.location"));
				filesToSend.add(file);
			}
			gedParameterDTO.setFilesToUpload(filesToSend);
			try {
				// if exist in ged
				List<GedDocumentDTO> existingDocumentInGed =
						gedClient.findDemandeDocuments(photoTag);
				if (!ACMValidationUtils.isNullOrEmpty(existingDocumentInGed)) {
					logger.error(CommonLoggerMessage.FILE_EXIST_ALREADY,
							File.class.getSimpleName());
					for (GedDocumentDTO gedDocumentDTO : existingDocumentInGed) {
						gedClient.delete(gedDocumentDTO.getId());
					}
				}
			}
			catch (Exception e) {
				logger.error("Failed to get FILE from GED");
				e.printStackTrace();
			}
			// send to GED
			try {
				// upload file to ged
				List<String> ids = gedClient.uploadToGed(gedParameterDTO);
				String idDocumentGED =
						ACMValidationUtils.isNullOrEmpty(ids) ? null : ids.get(0).split(";")[0];
				logger.info("Photo customer inserted in GED with ID : {} ", idDocumentGED);
				// save data in acm environnement
				AcmEnvironnementDTO acmEnvironnementDTO =
						acmEnvironnementService.find("REPORT_CLIENT_LOGO");
				// create new acmEnvironnementDTO if already not exist
				if (ACMValidationUtils.isNullOrEmpty(acmEnvironnementDTO)) {
					AcmEnvironnementDTO newAcmEnvironnementDTO = new AcmEnvironnementDTO();
					newAcmEnvironnementDTO.setKey("REPORT_CLIENT_LOGO");
					newAcmEnvironnementDTO.setValue(idDocumentGED);
					newAcmEnvironnementDTO.setCategory(SettingCategory.FUNCTIONAL.name());
					acmEnvironnementService.save(newAcmEnvironnementDTO);
				}
				// update acmEnvironnementDTO if already exist
				else if (!ACMValidationUtils.isNullOrEmpty(acmEnvironnementDTO)
						&& !ACMValidationUtils.isNullOrEmpty(acmEnvironnementDTO.getId())) {
					acmEnvironnementDTO.setValue(idDocumentGED);
					acmEnvironnementService.save(acmEnvironnementDTO.getId(), acmEnvironnementDTO);
				}
			}
			catch (Exception e) {
				logger.error("failed to save Photo in GED");
				logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
						e.getMessage());
			}
		}
	}

}
