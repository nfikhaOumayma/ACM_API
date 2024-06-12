/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import org.springframework.web.multipart.MultipartFile;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.AcmDocumentsDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.LoansDocumentsDTO;
import com.acm.utils.dtos.pagination.AcmDocumentsPaginationDTO;

/**
 * {@link AcmDocumentsService} interface.
 *
 * @author HaythemBenizid
 * @since 0.7.0
 */
public interface AcmDocumentsService {

	/**
	 * Find {@link AcmDocumentsDTO} by given ID.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @return the acmDocuments DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	AcmDocumentsDTO find(Long id) throws ResourcesNotFoundException;

	/**
	 * Find {@link List} of {@link AcmDocumentsDTO} by given params.
	 * 
	 * @author HaythemBenizid
	 * @param acmDocumentsDTO the acmDocuments DTO
	 * @return the list
	 */
	List<AcmDocumentsDTO> find(AcmDocumentsDTO acmDocumentsDTO);

	/**
	 * Find {@link AcmDocumentsPaginationDTO} by page size & page number & given params
	 * ({@link AcmDocumentsPaginationDTO}).
	 * 
	 * @author HaythemBenizid
	 * @param acmDocumentsPaginationDTO the loan pagination DTO
	 * @return the list
	 */
	AcmDocumentsPaginationDTO find(AcmDocumentsPaginationDTO acmDocumentsPaginationDTO);

	/**
	 * The method used for saving the given {@link AcmDocumentsDTO}.
	 * 
	 * @author HaythemBenizid
	 * @param acmDocumentsDTO the acmDocuments DTO
	 * @return the acmDocuments DTO
	 */
	AcmDocumentsDTO save(AcmDocumentsDTO acmDocumentsDTO);

	/**
	 * The method used for updating the given {@link AcmDocumentsDTO} by ID.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @param acmDocumentsDTO the acmDocuments DTO
	 * @return the acmDocuments DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	AcmDocumentsDTO save(Long id, AcmDocumentsDTO acmDocumentsDTO)
			throws ResourcesNotFoundException;

	/**
	 * Save all.
	 *
	 * @param acmDocumentsDTOs the acm documents DT os
	 * @return the acm documents DTO
	 */
	AcmDocumentsDTO saveAll(List<AcmDocumentsDTO> acmDocumentsDTOs);

	/**
	 * Delete {@link AcmDocumentsDTO} by given params.
	 * 
	 * @author HaythemBenizid
	 * @param acmDocumentsDTO the acmDocuments DTO
	 */
	void delete(AcmDocumentsDTO acmDocumentsDTO);

	/**
	 * The method used for saving the given {@link AcmDocumentsDTO} in database and MultipartFile
	 * uploadedFiles IN GED.
	 * 
	 * @author AbdelkarimTurki
	 * @param uploadedFiles the uploaded files
	 * @param acmDocumentsDTO the loan documents DTO
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	AcmDocumentsDTO save(MultipartFile[] uploadedFiles, AcmDocumentsDTO acmDocumentsDTO)
			throws ResourcesNotFoundException;

	/**
	 * The method saveImageToGed used for saving the given Photo in GED.
	 *
	 * @author AbdelkarimTurki
	 * @param uploadedFiles the uploaded files
	 * @param loanDTO the loan DTO
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	AcmDocumentsDTO save(MultipartFile[] uploadedFiles, LoanDTO loanDTO)
			throws ResourcesNotFoundException;

	/**
	 * Check required document for the given {@link AcmDocumentsDTO} by loanDTO.
	 *
	 * @author YesserSomai
	 * @param loanDTO the loan given
	 * @return Boolean TRUE if all document required exist FALSE else
	 */
	Boolean checkRequiredDocument(LoanDTO loanDTO);

	/**
	 * The method disable document in database.
	 *
	 * @author AbdelkarimTurki
	 * @param acmDocumentsDTO the acmDocuments DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	void disableDocument(AcmDocumentsDTO acmDocumentsDTO) throws ResourcesNotFoundException;

	/**
	 * Check required document upload signed for the given {@link AcmDocumentsDTO} by loanDTO.
	 *
	 * @author MoezMhiri
	 * @param loanDTO the loan given
	 * @return Boolean TRUE if all document required exist FALSE else
	 */
	Boolean checkRequiredDocumentSigned(LoanDTO loanDTO);

	/**
	 * Find documents by customer ID group by Loan.
	 * 
	 * @author HaythemBenizid
	 * @param acmDocumentsDTO the acm documents DTO
	 * @return the list
	 */
	List<LoansDocumentsDTO> findLoansDocumentsByCustomer(AcmDocumentsDTO acmDocumentsDTO);

	/**
	 * pars & Save received document in ACM DB & in GED.
	 *
	 * @author HaythemBenizid
	 * @param uploadedFiles the uploaded files
	 * @param documentsLoanDTOs the documents loan DT os
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	List<AcmDocumentsDTO> saveToGed(MultipartFile[] uploadedFiles, String documentsLoanDTOs)
			throws ResourcesNotFoundException;

	/**
	 * Find expenses document.
	 *
	 * @author ManelLamloum
	 * @param acmDocumentsDTO the acm documents DTO
	 * @return the list
	 */
	List<AcmDocumentsDTO> findExpensesDocument(AcmDocumentsDTO acmDocumentsDTO);

	/**
	 * updateListDocument.
	 *
	 * @author mkhemissi
	 * @param acmDocumentsDTOs the acm documents DT os
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	List<AcmDocumentsDTO> updateListDocument(List<AcmDocumentsDTO> acmDocumentsDTOs)
			throws ResourcesNotFoundException;

	/**
	 * Find history document.
	 *
	 * @author kouali
	 * @param typeDocId the type doc id
	 * @param loanId the loan id
	 * @param docIndex the doc index
	 * @return the list
	 */
	List<AcmDocumentsDTO> findHistoryDocument(long typeDocId, long loanId, Integer docIndex);

	/**
	 * Find history document by customer id.
	 *
	 * @author kouali
	 * @param typeDocId the type doc id
	 * @param custumerId the custumer id
	 * @param docIndex the doc index
	 * @return the list
	 */
	List<AcmDocumentsDTO> findHistoryDocumentByCustomerId(long typeDocId, long custumerId,
			Integer docIndex);

	/**
	 * Findhistory document by collection step.
	 *
	 * @param typeDocId the type doc id
	 * @param collectionStepId the collection step id
	 * @param docIndex the doc index
	 * @return the list
	 */
	List<AcmDocumentsDTO> findhistoryDocumentByCollectionStep(long typeDocId, long collectionStepId,
			Integer docIndex);

	/**
	 * Find in id Ib documents.
	 *
	 * @param idIbDocuments the id ib documents
	 * @return the list
	 */
	List<AcmDocumentsDTO> findInIdIbDocuments(List<Long> idIbDocuments);

	/**
	 * Find in setting document type ids.
	 *
	 * @param documentsDTO the documents DTO
	 * @param settingDocumentTypeIds the setting document type ids
	 * @return the list
	 */
	List<AcmDocumentsDTO> findInSettingDocumentTypeIds(AcmDocumentsDTO documentsDTO,
			List<Long> settingDocumentTypeIds);

	/**
	 * Findhistory document by item instance step.
	 *
	 * @param typeDocId the type doc id
	 * @param itemInstanceStepId the item instance step id
	 * @return the list
	 */
	List<AcmDocumentsDTO> findhistoryDocumentByItemInstanceStep(long typeDocId,
			long itemInstanceStepId);

	/**
	 * Findhistory document by element category.
	 *
	 * @param typeDocId the type doc id
	 * @param elementId the element id
	 * @param category the category
	 * @return the list
	 */
	List<AcmDocumentsDTO> findhistoryDocumentByElementCategory(long typeDocId, long elementId,
			String category);
}
