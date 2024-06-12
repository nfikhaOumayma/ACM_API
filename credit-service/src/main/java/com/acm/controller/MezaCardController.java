/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.io.IOException;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.exceptions.type.SaveFileException;
import com.acm.service.MezaCardService;
import com.acm.utils.dtos.AcmMezaCardDTO;
import com.acm.utils.dtos.pagination.AcmMezaCardPaginationDTO;
import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;

/**
 * {@link MezaCardController } class.
 *
 * @author YesserSomai
 * @since 1.0.5
 */
@RestController
@RequestMapping("/meza-card")
public class MezaCardController {

	/** The meza card service. */
	@Autowired
	private MezaCardService mezaCardService;

	/**
	 * Upload file.
	 *
	 * @param uploadedFiles the uploaded files
	 * @param branchDTO the branch DTO
	 * @param activate the activate
	 * @throws SaveFileException the save file exception
	 * @throws JsonParseException the json parse exception
	 * @throws JsonMappingException the json mapping exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	@PostMapping("/upload-meza-card-file")
	public void uploadFile(@RequestParam("uploadedFiles") MultipartFile[] uploadedFiles,
			@RequestParam("branchDTO") String branchDTO, @RequestParam("activate") Boolean activate)
			throws SaveFileException, JsonParseException, JsonMappingException, IOException {

		mezaCardService.uploadFile(uploadedFiles, branchDTO, activate);
	}

	/**
	 * Find pagination.
	 *
	 * @author YesserSomai
	 * @param acmMezaCardPaginationDTO the acm meza card pagination DTO
	 * @return the groupe pagination DTO
	 */
	@PostMapping("/find-pagination")
	public AcmMezaCardPaginationDTO findPagination(
			@RequestBody AcmMezaCardPaginationDTO acmMezaCardPaginationDTO) {

		return mezaCardService.find(acmMezaCardPaginationDTO);
	}

	/**
	 * Save Meza Cards.
	 *
	 * @author YesserSomai
	 * @param acmMezaCardDTOs the acm meza card DT os
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/update-cards")
	public List<AcmMezaCardDTO> save(@RequestBody List<AcmMezaCardDTO> acmMezaCardDTOs)
			throws ResourcesNotFoundException {

		return mezaCardService.save(acmMezaCardDTOs);
	}

	/**
	 * Find AcmMezaCardDTO.
	 *
	 * @author ManelLamloum
	 * @param acmMezaCardDTO the acm meza card DTO
	 * @return the acm meza card DTO
	 */
	@PostMapping("/find-first-order-by-cardNumber")
	public AcmMezaCardDTO findFirstOrderByAccount(@RequestBody AcmMezaCardDTO acmMezaCardDTO) {

		return mezaCardService.findByBranchIDAndStatus(acmMezaCardDTO);
	}

	/**
	 * Update data.
	 *
	 * @author ManelLamloum
	 * @param acmMezaCardDTO the acm meza card DTO
	 * @return the acm meza card DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update")
	public AcmMezaCardDTO update(@RequestBody AcmMezaCardDTO acmMezaCardDTO)
			throws ResourcesNotFoundException {

		return mezaCardService.save(acmMezaCardDTO.getIdMezaCard(), acmMezaCardDTO);
	}

	/**
	 * Update status to activate or assign to customer.
	 *
	 * @author ManelLamloum
	 * @param acmMezaCardDTO the acm meza card DTO
	 * @return the acm meza card DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/update-status-customer")
	public AcmMezaCardDTO updateStatusCustomer(@RequestBody AcmMezaCardDTO acmMezaCardDTO)
			throws ResourcesNotFoundException {

		return mezaCardService.update(acmMezaCardDTO);
	}

	/**
	 * Find.
	 * 
	 * @author ManelLamoum
	 * @param acmMezaCardDTO the acm meza card DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<AcmMezaCardDTO> find(@RequestBody AcmMezaCardDTO acmMezaCardDTO) {

		return mezaCardService.find(acmMezaCardDTO);
	}

	/**
	 * Generate MEZA CARD report.
	 * 
	 * @author HaythemBenizid
	 * @param acmMezaCardDTO the acm meza card DTO
	 * @return the byte[]
	 */
	@PostMapping("/report-meza-card")
	public byte[] generateMezaCardReport(@RequestBody AcmMezaCardDTO acmMezaCardDTO) {

		return mezaCardService.downloadReport(acmMezaCardDTO);
	}

	/**
	 * Find by id.
	 *
	 * @author MOEZ
	 * @param id the id
	 * @return the acm meza card DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@GetMapping("/{id}")
	public AcmMezaCardDTO findById(@PathVariable("id") Long id) throws ResourcesNotFoundException {

		return mezaCardService.find(id);
	}

}
