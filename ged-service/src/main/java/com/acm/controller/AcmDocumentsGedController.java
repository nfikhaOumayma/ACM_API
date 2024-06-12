/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.io.FileNotFoundException;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.acm.exceptions.type.GEDException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.AcmDocumentsGedService;
import com.acm.utils.dtos.AcmDocumentsGedDTO;

/**
 * This class @{link AcmDocumentsGedController} used to control all the AcmDocumentsGed requests.
 *
 * @author HaythemBenizid
 * @since 1.1.3
 */
@RestController
@RequestMapping("/documents-ged")
public class AcmDocumentsGedController {

	/** The AcmDocumentsGed service. */
	@Autowired
	private AcmDocumentsGedService acmDocumentsGedService;

	/**
	 * Find list acmDocumentsGed by given params.
	 *
	 * @author HaythemBenizid
	 * @param documentsLoanDTO the documents loan DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<AcmDocumentsGedDTO> find(@RequestBody AcmDocumentsGedDTO documentsLoanDTO) {

		return acmDocumentsGedService.find(documentsLoanDTO);
	}

	/**
	 * disable all the backup document.
	 *
	 * @author HaythemBenizid
	 * @return the integer
	 */
	@GetMapping("/disable-all")
	public Integer disableAll() {

		return acmDocumentsGedService.disableAll();
	}

	/**
	 * Count all.
	 * 
	 * @author HaythemBenizid
	 * @return the long
	 */
	@GetMapping("/count-all")
	public Long countAll() {

		return acmDocumentsGedService.countAll();
	}

	/**
	 * Upload client photo.
	 * 
	 * @author idridi
	 * @param photo the photo
	 * @param idDocument the id document
	 * @return the byte[]
	 * @throws GEDException the GED exception
	 * @throws FileNotFoundException the file not found exception
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping(path = "/upload-photo-client/")
	public byte[] uploadClientPhoto(@RequestBody MultipartFile photo, @RequestParam Long idDocument)
			throws GEDException, FileNotFoundException, ResourcesNotFoundException {

		return acmDocumentsGedService.uploadClientPhoto(photo, idDocument);
	}

	/**
	 * Find photo client.
	 * 
	 * @author idridi
	 * @param idDocument the id document
	 * @return the byte[]
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@GetMapping("/photo-client/{idDocument}")
	public byte[] findPhotoClient(@PathVariable("idDocument") Long idDocument)
			throws ResourcesNotFoundException {

		return acmDocumentsGedService.findPhotoClient(idDocument);
	}
}
