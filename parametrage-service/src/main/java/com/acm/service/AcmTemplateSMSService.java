/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.AcmTemplateSMSDTO;

/**
 * The Interface AcmTemplateSMSService.
 */
public interface AcmTemplateSMSService {

	/**
	 * Find.
	 *
	 * @param acmTemplateSMSDTO the acm template SMSDTO
	 * @return the list
	 */

	/**
	 * Save.
	 *
	 * @param acmTemplateSMSDTO the acm template SMSDTO
	 * @return the acm template SMSDTO
	 */

	AcmTemplateSMSDTO save(AcmTemplateSMSDTO acmTemplateSMSDTO);

	/**
	 * Update message body.
	 *
	 * @param acmTemplateSMSDTO the acm template SMSDTO
	 * @return the acm template SMSDTO
	 */
	AcmTemplateSMSDTO updateMessageBody(AcmTemplateSMSDTO acmTemplateSMSDTO);

	/**
	 * Find by code.
	 *
	 * @param acmTemplateSMSDTO the acm template SMSDTO
	 * @return the acm template SMSDTO
	 */

	List<AcmTemplateSMSDTO> find(AcmTemplateSMSDTO acmTemplateSMSDTO);

	/**
	 * Find by code.
	 *
	 * @param acmTemplateSMSDTO the acm template SMSDTO
	 * @return the acm template SMSDTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */

	AcmTemplateSMSDTO findByCode(AcmTemplateSMSDTO acmTemplateSMSDTO)
			throws ResourcesNotFoundException;

}
