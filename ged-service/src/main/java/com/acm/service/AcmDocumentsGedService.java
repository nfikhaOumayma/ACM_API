/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import org.springframework.web.multipart.MultipartFile;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.AcmDocumentsGedDTO;

/**
 * {@link AcmDocumentsGedService} interface.
 *
 * @author HaythemBenizid
 * @since 1.1.3
 */
public interface AcmDocumentsGedService {

	/**
	 * Find {@link AcmDocumentsGedDTO} by given ID.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @return the acmDocuments DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	AcmDocumentsGedDTO find(Long id) throws ResourcesNotFoundException;

	/**
	 * Find {@link List} of {@link AcmDocumentsGedDTO} by given params.
	 * 
	 * @author HaythemBenizid
	 * @param acmDocumentsGedDTO the acmDocuments DTO
	 * @return the list
	 */
	List<AcmDocumentsGedDTO> find(AcmDocumentsGedDTO acmDocumentsGedDTO);

	/**
	 * The method used for saving the given {@link AcmDocumentsGedDTO}.
	 * 
	 * @author HaythemBenizid
	 * @param acmDocumentsGedDTO the acmDocuments DTO
	 * @return the acmDocuments DTO
	 */
	AcmDocumentsGedDTO save(AcmDocumentsGedDTO acmDocumentsGedDTO);

	/**
	 * Delete {@link AcmDocumentsGedDTO} by given params.
	 * 
	 * @author HaythemBenizid
	 * @param acmDocumentsGedDTO the acmDocuments DTO
	 */
	void delete(AcmDocumentsGedDTO acmDocumentsGedDTO);

	/**
	 * Insert all backup document to GED && delete from ACM DB .
	 *
	 * @author HaythemBenizid
	 * @return the integer
	 */
	Integer disableAll();

	/**
	 * Count all.
	 * 
	 * @author HaythemBenizid
	 * @return the long
	 */
	Long countAll();

	/**
	 * Upload client photo.
	 * 
	 * @author idridi
	 * @param photo the photo
	 * @param idDocument the id document
	 * @return the byte[]
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	byte[] uploadClientPhoto(MultipartFile photo, Long idDocument)
			throws ResourcesNotFoundException;

	/**
	 * Find photo client.
	 * 
	 * @author idridi
	 * @param idDocument the id document
	 * @return the byte[]
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	byte[] findPhotoClient(Long idDocument) throws ResourcesNotFoundException;

	/**
	 * Save.
	 * 
	 * @author idridi
	 * @param id the id
	 * @param acmDocumentsGedDTO the acm documents ged DTO
	 * @return the acm documents ged DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	AcmDocumentsGedDTO save(Long id, AcmDocumentsGedDTO acmDocumentsGedDTO)
			throws ResourcesNotFoundException;

}
