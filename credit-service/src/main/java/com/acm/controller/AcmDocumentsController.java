/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.io.IOException;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.acm.exceptions.type.GEDException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.AcmDocumentsService;
import com.acm.utils.dtos.AcmDocumentsDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.LoansDocumentsDTO;
import com.acm.utils.dtos.pagination.AcmDocumentsPaginationDTO;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * This class @{link DocumentsLoanController} used to control all the AcmDocuments requests.
 *
 * @author HaythemBenizid
 * @since 0.7.0
 */
@RestController
@RequestMapping("/loans-documents")
public class AcmDocumentsController {

	/** The AcmDocuments service. */
	@Autowired
	private AcmDocumentsService documentsLoanService;

	/**
	 * Find document by id.
	 * 
	 * @author HaythemBenizid
	 * @param id the id
	 * @return the documents loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@GetMapping("/{id}")
	public AcmDocumentsDTO findById(@PathVariable("id") Long id) throws ResourcesNotFoundException {

		return documentsLoanService.find(id);
	}

	/**
	 * Find list document by given params.
	 * 
	 * @author HaythemBenizid
	 * @param documentsLoanDTO the documents loan DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<AcmDocumentsDTO> find(@RequestBody AcmDocumentsDTO documentsLoanDTO) {

		return documentsLoanService.find(documentsLoanDTO);
	}

	/**
	 * Find loans documents by customer ID group by Loan.
	 * 
	 * @author HaythemBenizid
	 * @param documentsLoanDTO the documents loan DTO
	 * @return the list
	 */
	@PostMapping("/find-documents-customer")
	public List<LoansDocumentsDTO> findLoansDocumentsByCustomer(
			@RequestBody AcmDocumentsDTO documentsLoanDTO) {

		return documentsLoanService.findLoansDocumentsByCustomer(documentsLoanDTO);
	}

	/**
	 * Find pagination.
	 * 
	 * @author HaythemBenizid
	 * @param acmDocumentsPaginationDTO the loan pagination DTO
	 * @return the loan pagination DTO
	 */
	@PostMapping("/find-pagination")
	public AcmDocumentsPaginationDTO findPagination(
			@RequestBody AcmDocumentsPaginationDTO acmDocumentsPaginationDTO) {

		return documentsLoanService.find(acmDocumentsPaginationDTO);
	}

	/**
	 * insert the documents.
	 * 
	 * @author HaythemBenizid
	 * @param documentsLoanDTO the documents loan DTO
	 * @return the documents loan DTO
	 */
	@PostMapping("/create")
	public AcmDocumentsDTO create(@RequestBody AcmDocumentsDTO documentsLoanDTO) {

		return documentsLoanService.save(documentsLoanDTO);
	}

	/**
	 * Update the AcmDocuments by id.
	 *
	 * @author HaythemBenizid
	 * @param acmDocumentsDTO the acmDocuments DTO
	 * @return the acmDocuments DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update")
	public AcmDocumentsDTO update(@RequestBody AcmDocumentsDTO acmDocumentsDTO)
			throws ResourcesNotFoundException {

		return documentsLoanService.save(acmDocumentsDTO.getIdDocument(), acmDocumentsDTO);
	}

	/**
	 * delete document from database.
	 *
	 * @author AbdelkarimTurki
	 * @param id the id
	 */
	@DeleteMapping("/delete/{id}")
	public void delete(@PathVariable("id") Long id) {

		documentsLoanService.delete(new AcmDocumentsDTO(id));
	}

	/**
	 * Save to GED.
	 *
	 * @author AbdelkarimTurki
	 * @author HaythemBenizid
	 * @param uploadedFiles the uploaded files
	 * @param documentsLoanDTO the documents loan DTO
	 * @return the list
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/saveToGed")
	public List<AcmDocumentsDTO> saveToGed(
			@RequestParam("uploadedFiles") MultipartFile[] uploadedFiles,
			@RequestParam("documentsLoanDTO") String documentsLoanDTO)
			throws IOException, ResourcesNotFoundException {

		return documentsLoanService.saveToGed(uploadedFiles, documentsLoanDTO);
	}

	/**
	 * Save to GED.
	 *
	 * @author AbdelkarimTurki
	 * @param uploadedFiles the uploaded files
	 * @param documentsLoanDTO the documents loan DTO
	 * @return the list
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/save-image-to-ged")
	public AcmDocumentsDTO saveImageToGed(
			@RequestParam("uploadedFiles") MultipartFile[] uploadedFiles,
			@RequestParam("documentsLoanDTO") String documentsLoanDTO)
			throws IOException, ResourcesNotFoundException {

		ObjectMapper mapper = new ObjectMapper();
		LoanDTO loanDTO = mapper.readValue(documentsLoanDTO, LoanDTO.class);
		return documentsLoanService.save(uploadedFiles, loanDTO);
	}

	/**
	 * disable document from database.
	 *
	 * @author AbdelkarimTurki
	 * @param documentsLoanDTO the documents loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/disable-document")
	public void disableDocument(@RequestBody AcmDocumentsDTO documentsLoanDTO)
			throws ResourcesNotFoundException {

		documentsLoanService.disableDocument(documentsLoanDTO);
	}

	/**
	 * Find expenses document.
	 * 
	 * @author ManelLamloum
	 * @param documentsLoanDTO the documents loan DTO
	 * @return the list
	 */
	@PostMapping("/find-expenses-document")
	public List<AcmDocumentsDTO> findExpensesDocument(
			@RequestBody AcmDocumentsDTO documentsLoanDTO) {

		return documentsLoanService.findExpensesDocument(documentsLoanDTO);
	}

	/**
	 * Update list document.
	 * 
	 * @author mkhemissi
	 * @param documentsLoanDTOs the documents loan DT os
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/update-document-list")
	public List<AcmDocumentsDTO> updateListDocument(
			@RequestBody List<AcmDocumentsDTO> documentsLoanDTOs)
			throws ResourcesNotFoundException {

		return documentsLoanService.updateListDocument(documentsLoanDTOs);
	}

	/**
	 * Findhistory document.
	 *
	 * @param typeDocId the type doc id
	 * @param loanId the loan id
	 * @param docIndex the doc index
	 * @return the list
	 * @throws GEDException the GED exception
	 */
	@GetMapping("/history/{typeDocId}/{loanId}/{docIndex}")
	public List<AcmDocumentsDTO> findhistoryDocument(@PathVariable("typeDocId") long typeDocId,
			@PathVariable("loanId") long loanId, @PathVariable("docIndex") Integer docIndex)
			throws GEDException {

		return documentsLoanService.findHistoryDocument(typeDocId, loanId, docIndex);
	}

	/**
	 * Findhistory document by customer id.
	 *
	 * @param typeDocId the type doc id
	 * @param custumerId the custumer id
	 * @param docIndex the doc index
	 * @return the list
	 * @throws GEDException the GED exception
	 */
	@GetMapping("/history/customer/{typeDocId}/{custumerId}/{docIndex}")
	public List<AcmDocumentsDTO> findhistoryDocumentByCustomerId(
			@PathVariable("typeDocId") long typeDocId, @PathVariable("custumerId") long custumerId,
			@PathVariable("docIndex") Integer docIndex) throws GEDException {

		return documentsLoanService.findHistoryDocumentByCustomerId(typeDocId, custumerId,
				docIndex);
	}

	/**
	 * Findhistory document by collection step.
	 *
	 * @param typeDocId the type doc id
	 * @param loanId the loan id
	 * @param docIndex the doc index
	 * @return the list
	 * @throws GEDException the GED exception
	 */
	@GetMapping("/history/collectionStep/{typeDocId}/{collectionStepId}/{docIndex}")
	public List<AcmDocumentsDTO> findhistoryDocumentByCollectionStep(
			@PathVariable("typeDocId") long typeDocId,
			@PathVariable("collectionStepId") long loanId,
			@PathVariable("docIndex") Integer docIndex) throws GEDException {

		return documentsLoanService.findhistoryDocumentByCollectionStep(typeDocId, loanId,
				docIndex);
	}

	/**
	 * Findhistory document by item step.
	 *
	 * @param typeDocId the type doc id
	 * @param itemInstanceStepId the item instance step id
	 * @return the list
	 * @throws GEDException the GED exception
	 */
	@GetMapping("/history/itemStep/{typeDocId}/{itemInstanceStepId}")
	public List<AcmDocumentsDTO> findhistoryDocumentByItemStep(
			@PathVariable("typeDocId") long typeDocId,
			@PathVariable("itemInstanceStepId") long itemInstanceStepId) throws GEDException {

		return documentsLoanService.findhistoryDocumentByItemInstanceStep(typeDocId,
				itemInstanceStepId);
	}

	/**
	 * Findhistory document by category element.
	 *
	 * @param typeDocId the type doc id
	 * @param elementId the element id
	 * @param category the category
	 * @return the list
	 * @throws GEDException the GED exception
	 */
	@GetMapping("/history/element/{typeDocId}/{elementId}/{category}")
	public List<AcmDocumentsDTO> findhistoryDocumentByCategoryElement(
			@PathVariable("typeDocId") long typeDocId, @PathVariable("elementId") long elementId,
			@PathVariable("category") String category) throws GEDException {

		return documentsLoanService.findhistoryDocumentByElementCategory(typeDocId, elementId,
				category);
	}
}
