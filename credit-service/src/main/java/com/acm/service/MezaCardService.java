/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.io.IOException;
import java.util.List;

import org.springframework.web.multipart.MultipartFile;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.exceptions.type.SaveFileException;
import com.acm.utils.dtos.AcmMezaCardDTO;
import com.acm.utils.dtos.pagination.AcmMezaCardPaginationDTO;
import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;

/**
 * {@link MezaCardService} class.
 *
 * @author YesserSomai
 * @since 1.0.5
 */
public interface MezaCardService {

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
	void uploadFile(MultipartFile[] uploadedFiles, String branchDTO, Boolean activate)
			throws SaveFileException, JsonParseException, JsonMappingException, IOException;

	/**
	 * Download EXCEL report.
	 * 
	 * @author HaythemBenizid
	 * @param acmMezaCardDTO the acm meza card DTO
	 * @return the byte[]
	 */
	byte[] downloadReport(AcmMezaCardDTO acmMezaCardDTO);

	/**
	 * Find by pagination.
	 *
	 * @author YesserSomai
	 * @param acmMezaCardPaginationDTO the acm meza card pagination DTO
	 * @return the acm meza card pagination DTO
	 */
	AcmMezaCardPaginationDTO find(AcmMezaCardPaginationDTO acmMezaCardPaginationDTO);

	/**
	 * Save.
	 *
	 * @author YesserSomai
	 * @param acmMezaCardDTOs the acm meza card DT os
	 * @return the list of Acm Meza Card DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	List<AcmMezaCardDTO> save(List<AcmMezaCardDTO> acmMezaCardDTOs)
			throws ResourcesNotFoundException;

	/**
	 * Find by branch ID and status.
	 *
	 * @author ManelLamloum
	 * @param acmMezaCardDTO the acm meza card DTO
	 * @return the acm meza card DTO
	 */
	AcmMezaCardDTO findByBranchIDAndStatus(AcmMezaCardDTO acmMezaCardDTO);

	/**
	 * Save.
	 *
	 * @author ManelLamloum
	 * @param id the id
	 * @param acmMezaCardDTO the acm meza card DTO
	 * @return the acm meza card DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	AcmMezaCardDTO save(Long id, AcmMezaCardDTO acmMezaCardDTO) throws ResourcesNotFoundException;

	/**
	 * update status or assign to customer.
	 *
	 * @author ManelLamloum
	 * @param acmMezaCardDTO the acm meza card DTO
	 * @return the acm meza card DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	AcmMezaCardDTO update(AcmMezaCardDTO acmMezaCardDTO) throws ResourcesNotFoundException;

	/**
	 * Find by given params.
	 * 
	 * @author ManelLamloum
	 * @param acmMezaCardDTO the acm meza card DTO
	 * @return the list
	 */
	List<AcmMezaCardDTO> find(AcmMezaCardDTO acmMezaCardDTO);

	/**
	 * Find by ID.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @return the acm meza card DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	AcmMezaCardDTO find(Long id) throws ResourcesNotFoundException;

	/**
	 * Update status.
	 *
	 * @author ManelLamloum
	 * @param acmMezaCardDTO the acm meza card DTO
	 * @return the acm meza card DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	AcmMezaCardDTO updateStatus(AcmMezaCardDTO acmMezaCardDTO) throws ResourcesNotFoundException;
}
