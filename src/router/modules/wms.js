// 路由：WMS
export default {
  id: 5,
  name: 'WMS',
  children: [
    {
      path: '/wms/inbound-application',
      component: 'Layout',
      hidden: false,
      name: 'WMSInboundApplication',
      alwaysShow: false,
      redirect: '/wms/material-inbound/raw-material/application/steel/index',
      meta: { title: '入库办理', icon: 'contract', noCache: true },
      children: [
        {
          name: 'SteelInboundApplication',
          path: 'steel',
          hidden: false,
          component: '/wms/material-inbound/raw-material/application/steel/index',
          meta: { title: '钢材入库', icon: 'project', noCache: true }
        },
        {
          name: 'AuxMatInboundApplication',
          path: 'auxiliary-material',
          hidden: false,
          component: '/wms/material-inbound/raw-material/application/auxiliary-material/index',
          meta: { title: '辅材入库', icon: 'project', noCache: true }
        },
        {
          name: 'ManufInboundApplication',
          path: 'manufactured',
          hidden: false,
          component: '/wms/material-inbound/manufactured/index',
          meta: { title: '制成品入库', icon: 'project', noCache: true }
        },
        {
          name: 'OtherInboundApplication',
          path: 'other',
          hidden: false,
          component: '/wms/material-inbound/raw-material/application/other/index',
          meta: { title: '其它入库', icon: 'project', noCache: true }
        },
        {
          name: 'RawMatInboundApplicationRecord',
          path: 'material-inbound/raw-material/record',
          hidden: false,
          component: '/wms/material-inbound/raw-material/record/index',
          meta: { title: '入库申请', icon: 'project', noCache: true }
        }
      ]
    },
    {
      path: '/wms/return-application',
      component: 'Layout',
      hidden: false,
      name: 'WMSReturnApplication',
      alwaysShow: false,
      redirect: '/wms/material-return/raw-material/application/steel-plate/index',
      meta: { title: '退库办理', icon: 'contract', noCache: true },
      children: [
        {
          name: 'SteelPlateReturnApplication',
          path: 'steel-plate',
          hidden: false,
          component: '/wms/material-return/raw-material/application/steel-plate/index',
          meta: { title: '钢板退库', icon: 'project', noCache: true }
        },
        {
          name: 'SectionSteelReturnApplication',
          path: 'section-steel',
          hidden: false,
          component: '/wms/material-return/raw-material/application/section-steel/index',
          meta: { title: '型材退库', icon: 'project', noCache: true }
        },
        {
          name: 'SteelCoilReturnApplication',
          path: 'steel-coil',
          hidden: false,
          component: '/wms/material-return/raw-material/application/steel-coil/index',
          meta: { title: '钢卷退库', icon: 'project', noCache: true }
        },
        {
          name: 'AuxMatReturnApplication',
          path: 'auxiliary-material',
          hidden: false,
          component: '/wms/material-return/raw-material/application/auxiliary-material/index',
          meta: { title: '辅材退库', icon: 'project', noCache: true }
        },
        {
          name: 'OtherReturnApplication',
          path: 'other',
          hidden: false,
          component: '/wms/material-return/raw-material/application/other/index',
          meta: { title: '其它退库', icon: 'project', noCache: true }
        },
        {
          name: 'RawMatReturnApplicationRecord',
          path: 'material-return/raw-material/record',
          hidden: false,
          component: '/wms/material-return/raw-material/record/index',
          meta: { title: '退库申请', icon: 'project', noCache: true }
        }
      ]
    },
    {
      path: '/wms/reject-application',
      component: 'Layout',
      hidden: false,
      name: 'WMSRejectApplication',
      alwaysShow: true,
      redirect: '/wms/reject-application/steel-plate/index',
      meta: { title: '退货办理', icon: 'contract', noCache: true },
      children: [
        {
          name: 'SteelPlateRejectApplication',
          path: 'raw-mat',
          hidden: false,
          component: '/wms/material-reject/raw-material/application/index',
          meta: { title: '退货', icon: 'project', noCache: true }
        },
        {
          name: 'RawMatRejectRecord',
          path: 'reject-record/raw-mat',
          hidden: false,
          component: '/wms/material-reject/raw-material/record/index',
          meta: { title: '退货申请', icon: 'project', noCache: true }
        }
      ]
    },
    {
      path: '/wms/quality-inspection-manage',
      component: 'Layout',
      hidden: false,
      name: 'WMSQualityInspectionManage',
      alwaysShow: true,
      redirect: '/wms/quality-inspection-manage/inbound-inspection/index',
      meta: { title: '质检管理', icon: 'contract', noCache: true },
      children: [
        {
          name: 'InboundInspection',
          path: 'inbound-inspection',
          hidden: false,
          component: '/wms/quality-inspection-manage/inbound-inspection/index',
          meta: { title: '入库质检表', icon: 'project', noCache: true }
        },
        {
          name: 'InboundInspectionDetail',
          path: 'inbound-inspection-detail',
          hidden: false,
          component: '/wms/quality-inspection-manage/inbound-inspection-detail/index',
          meta: { title: '入库质检明细', icon: 'project', noCache: true }
        }
      ]
    },
    {
      path: '/wms/inbound-and-outbound-review',
      component: 'Layout',
      hidden: false,
      name: 'WMSInboundAndOutboundReview',
      alwaysShow: false,
      redirect: '/wms/material-inbound/raw-material/review/index',
      meta: { title: '出入库审核', icon: 'contract', noCache: true },
      children: [
        {
          name: 'RawMatInboundApplicationReview',
          path: 'material-inbound/raw-material/review',
          hidden: false,
          component: '/wms/material-inbound/raw-material/review/index',
          meta: { title: '入库审核', icon: 'project', noCache: true }
        },
        {
          name: 'RawMatTransferApplicationReview',
          path: 'material-transfer/raw-material/review',
          hidden: false,
          component: '/wms/material-transfer/raw-material/review/index',
          meta: { title: '调拨审核', icon: 'project', noCache: true }
        },
        {
          name: 'RawMatReturnApplicationReview',
          path: 'material-return/raw-material/review',
          hidden: false,
          component: '/wms/material-return/raw-material/review/index',
          meta: { title: '退库审核', icon: 'project', noCache: true }
        },
        {
          name: 'RawMatRejectReview',
          path: 'material-reject/raw-material/review',
          hidden: false,
          component: '/wms/material-reject/raw-material/review/index',
          meta: { title: '退货审核', icon: 'project', noCache: true }
        }
      ]
    },
    {
      path: '/wms/material-inventory',
      component: 'Layout',
      hidden: false,
      name: 'WMSMaterialInventory',
      alwaysShow: false,
      redirect: '/wms/material-inventory/steel/index',
      meta: { title: '物料仓', icon: 'contract', noCache: true },
      children: [
        {
          name: 'WMSMaterialLabelPrint',
          path: 'material-label-print',
          hidden: false,
          component: '/wms/material-label-print/index',
          meta: { title: '标签打印', icon: 'project', noCache: true }
        },
        {
          name: 'WMSSteelMaterialInventory',
          path: 'steel',
          hidden: false,
          component: '/wms/material-inventory/steel/index',
          meta: { title: '钢材仓库', icon: 'project', noCache: true }
        },
        {
          name: 'WMSAuxMaterialMaterialInventory',
          path: 'aux-material',
          hidden: false,
          component: '/wms/material-inventory/aux-material/index',
          meta: { title: '辅材仓库', icon: 'project', noCache: true }
        },
        {
          name: 'WMSOtherMaterialInventory',
          path: 'other',
          hidden: false,
          component: '/wms/material-inventory/other/index',
          meta: { title: '其它仓库', icon: 'project', noCache: true }
        },
        {
          name: 'WMSOutboundReview',
          path: 'material-outbound/raw-material/review',
          hidden: false,
          component: '/wms/material-outbound/raw-material/review/index',
          meta: { title: '出库审核', icon: 'project', noCache: true }
        },
        {
          name: 'WMSOutboundRecord',
          path: 'material-outbound/raw-material/record',
          hidden: false,
          component: '/wms/material-outbound/raw-material/record/index',
          meta: { title: '出库记录', icon: 'project', noCache: true }
        },
        {
          name: 'WMSPartyABorrowManage',
          path: 'material-transfer/raw-material/party-a-borrow-manage',
          hidden: false,
          component: '/wms/material-transfer/raw-material/party-a-borrow-manage/index',
          meta: { title: '甲供材料借出管理', icon: 'project', noCache: true }
        }
      ]
    },
    {
      path: '/wms/scrap-manage',
      component: 'Layout',
      hidden: false,
      name: 'WMSScrapManage',
      alwaysShow: true,
      redirect: '/wms/scrap-manage/steel/index',
      meta: { title: '废料管理', icon: 'contract', noCache: true },
      children: [
        {
          name: 'WMSSteelScrapList',
          path: 'steel-list',
          hidden: false,
          component: '/wms/scrap-manage/steel/index',
          meta: { title: '钢材列表', icon: 'project', noCache: true }
        }
      ]
    },
    {
      path: '/wms/material-freeze',
      component: 'Layout',
      hidden: false,
      name: 'WMSFreezeManage',
      alwaysShow: false,
      redirect: '/wms/material-freeze/raw-material/record/index',
      meta: { title: '冻结管理', icon: 'contract', noCache: true },
      children: [
        {
          name: 'WMSRawMatFreezeList',
          path: 'raw-mat-record',
          hidden: false,
          component: '/wms/material-freeze/raw-material/record/index',
          meta: { title: '冻结列表', icon: 'project', noCache: true }
        },
        {
          name: 'WMSRawMatUnfreezeRecord',
          path: 'raw-mat-unfreeze-record',
          hidden: false,
          component: '/wms/material-freeze/raw-material/unfreeze-record/index',
          meta: { title: '解冻记录', icon: 'project', noCache: true }
        }
      ]
    },
    {
      path: '/wms/report',
      component: 'Layout',
      hidden: false,
      name: 'WMSReport',
      alwaysShow: true,
      redirect: '/wms/report/raw-material',
      meta: { title: '报表中心', icon: 'contract', noCache: true },
      children: [
        {
          name: 'WMSRawMaterialReturnToPartyARecord',
          path: 'return-to-party-a',
          hidden: false,
          component: '/wms/operate-record/raw-material/return-to-party-a/index',
          meta: { title: '归还甲方', icon: 'project', noCache: true }
        },
        {
          name: 'WMSRawMaterialPartyABuyInRecord',
          path: 'party-a-buy-in',
          hidden: false,
          component: '/wms/operate-record/raw-material/party-a-buy-in/index',
          meta: { title: '甲供买入', icon: 'project', noCache: true }
        },
        {
          name: 'WMSReportRawMaterialMaterialInboundReceipt',
          path: 'report/raw-material/material-inbound-receipt',
          hidden: false,
          component: '/wms/report/raw-material/material-inbound-receipt/index',
          meta: { title: '入库单表', icon: 'project', noCache: true }
        },
        {
          name: 'WMSReportRawMaterialMaterialInboundDetails',
          path: 'report/raw-material/material-inbound-details',
          hidden: false,
          component: '/wms/report/raw-material/material-inbound-details/index',
          meta: { title: '入库明细', icon: 'project', noCache: true }
        },
        {
          name: 'WMSReportRawMaterialMaterialOutboundDetails',
          path: 'report/raw-material/material-outbound-details',
          hidden: false,
          component: '/wms/report/raw-material/material-outbound-details/index',
          meta: { title: '出库明细', icon: 'project', noCache: true }
        },
        {
          name: 'WMSReportRawMaterialMaterialReturnDetails',
          path: 'report/raw-material/material-return-details',
          hidden: false,
          component: '/wms/report/raw-material/material-return-details/index',
          meta: { title: '退库明细', icon: 'project', noCache: true }
        },
        {
          name: 'WMSReportRawMaterialMaterialTransferDetails',
          path: 'report/raw-material/material-transfer-details',
          hidden: false,
          component: '/wms/report/raw-material/material-transfer-details/index',
          meta: { title: '调拨明细', icon: 'project', noCache: true }
        },
        {
          name: 'WMSReportRawMaterialSupplementRecord',
          path: 'report/raw-material/supplement-record',
          hidden: false,
          component: '/wms/report/raw-material/supplement-record/index',
          meta: { title: '调整记录', icon: 'project', noCache: true }
        },
        {
          name: 'WMSReportRawMaterialMaterialRejectReceipt',
          path: 'report/raw-material/material-reject-receipt',
          hidden: false,
          component: '/wms/report/raw-material/material-reject-receipt/index',
          meta: { title: '退货单表', icon: 'project', noCache: true }
        },
        {
          name: 'WMSReportRawMaterialCutSurplusReceipt',
          path: 'report/raw-material/cut-surplus-receipt',
          hidden: false,
          component: '/wms/report/raw-material/cut-surplus-receipt/index',
          meta: { title: '切割余料表', icon: 'project', noCache: true }
        },
        {
          name: 'WMSReportRawMaterialMaterialInventory',
          path: 'report/raw-material/material-inventory',
          hidden: false,
          component: '/wms/report/raw-material/material-inventory/index',
          meta: { title: '库存报表', icon: 'project', noCache: true }
        },
        {
          name: 'WMSReportRawMaterialSendAndReceiveStorageReceipt',
          path: 'report/raw-material/send-and-receive-storage',
          hidden: false,
          component: '/wms/report/raw-material/send-and-receive-storage/index',
          meta: { title: '收发存报表', icon: 'project', noCache: true }
        },
        {
          name: 'WMSReportRawMaterialInventoryDetailReceipt',
          path: 'report/raw-material/inventory-detail',
          hidden: false,
          component: '/wms/report/raw-material/inventory-detail/index',
          meta: { title: '存货明细帐', icon: 'project', noCache: true }
        },
        {
          name: 'WMSReportRawMaterialPSIReceipt',
          path: 'report/raw-material/psi',
          hidden: false,
          component: '/wms/report/raw-material/psi/index',
          meta: { title: '进销存记录', icon: 'project', noCache: true }
        },
        {
          name: 'WMSReportRawMaterialHighTechResearchAndDevelopmentFee',
          path: 'report/raw-material/high-tech-rd-fee',
          hidden: false,
          component: '/wms/report/raw-material/high-tech-rd-fee/index',
          meta: { title: '高新研发费', icon: 'project', noCache: true }
        }
      ]
    },
    {
      path: '/wms/inventory-warning-manage',
      component: 'Layout',
      hidden: false,
      name: 'InventoryManage',
      alwaysShow: false,
      redirect: '/wms/inventory-warning',
      meta: { title: '库存管理', icon: 'contract', noCache: true },
      children: [
        {
          name: 'InventoryWarning',
          path: 'inventory-warning',
          hidden: false,
          component: '/wms/inventory-warning/index',
          meta: { title: '库存预警', icon: 'project', noCache: true }
        }
      ]
    }
  ]
}
