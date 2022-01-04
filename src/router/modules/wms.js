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
      redirect: '/wms/inbound-application/steel/index',
      meta: { title: '入库办理', icon: 'contract', noCache: true },
      children: [
        {
          name: 'SteelInboundApplication',
          path: 'steel',
          hidden: false,
          component: '/wms/inbound-application/steel/index',
          meta: { title: '钢材入库', icon: 'project', noCache: true }
        },
        {
          name: 'AuxMatInboundApplication',
          path: 'auxiliary-material',
          hidden: false,
          component: '/wms/inbound-application/auxiliary-material/index',
          meta: { title: '辅材入库', icon: 'project', noCache: true }
        },
        {
          name: 'GasInboundApplication',
          path: 'gas',
          hidden: false,
          component: '/wms/inbound-application/gas/index',
          meta: { title: '气体入库', icon: 'project', noCache: true }
        },
        {
          name: 'RawMatInboundApplicationRecord',
          path: 'inbound-application-record/raw-mat',
          hidden: false,
          component: '/wms/inbound-application-record/raw-mat/index',
          meta: { title: '原材料-入库申请', icon: 'project', noCache: true }
        }
      ]
    },
    {
      path: '/wms/return-application',
      component: 'Layout',
      hidden: false,
      name: 'WMSReturnApplication',
      alwaysShow: false,
      redirect: '/wms/return-application/steel-plate/index',
      meta: { title: '退库办理', icon: 'contract', noCache: true },
      children: [
        {
          name: 'SteelPlateReturnApplication',
          path: 'steel-plate',
          hidden: false,
          component: '/wms/return-application/steel-plate/index',
          meta: { title: '钢板退库', icon: 'project', noCache: true }
        },
        {
          name: 'SectionSteelReturnApplication',
          path: 'section-steel',
          hidden: false,
          component: '/wms/return-application/section-steel/index',
          meta: { title: '型材退库', icon: 'project', noCache: true }
        },
        {
          name: 'SteelCoilReturnApplication',
          path: 'steel-coil',
          hidden: false,
          component: '/wms/return-application/steel-coil/index',
          meta: { title: '钢卷退库', icon: 'project', noCache: true }
        },
        {
          name: 'AuxMatReturnApplication',
          path: 'auxiliary-material',
          hidden: false,
          component: '/wms/return-application/auxiliary-material/index',
          meta: { title: '辅材退库', icon: 'project', noCache: true }
        },
        {
          name: 'GasReturnApplication',
          path: 'gas',
          hidden: false,
          component: '/wms/return-application/gas/index',
          meta: { title: '气体退库', icon: 'project', noCache: true }
        },
        {
          name: 'RawMatReturnApplicationRecord',
          path: 'return-application-record/raw-mat',
          hidden: false,
          component: '/wms/return-application-record/raw-mat/index',
          meta: { title: '原材料-退库申请', icon: 'project', noCache: true }
        }
      ]
    },
    // {
    //   path: '/wms/reject-application',
    //   component: 'Layout',
    //   hidden: false,
    //   name: 'WMSRejectApplication',
    //   alwaysShow: true,
    //   redirect: '/wms/reject-application/steel-plate/index',
    //   meta: { title: '退货办理', icon: 'contract', noCache: true },
    //   children: [
    //     {
    //       name: 'SteelPlateRejectApplication',
    //       path: 'raw-mat',
    //       hidden: false,
    //       component: '/wms/reject-application/raw-mat/index',
    //       meta: { title: '原材料-退货', icon: 'project', noCache: true }
    //     }
    //   ]
    // },
    {
      path: '/wms/inbound-and-outbound-review',
      component: 'Layout',
      hidden: false,
      name: 'WMSInboundAndOutboundReview',
      alwaysShow: false,
      redirect: '/wms/inbound-application-review/raw-mat/index',
      meta: { title: '出入库审核', icon: 'contract', noCache: true },
      children: [
        {
          name: 'RawMatInboundApplicationReview',
          path: 'inbound-application-review/raw-mat',
          hidden: false,
          component: '/wms/inbound-application-review/raw-mat/index',
          meta: { title: '原材料-入库审核', icon: 'project', noCache: true }
        },
        {
          name: 'RawMatTransferApplicationReview',
          path: 'transfer-application-review/raw-mat',
          hidden: false,
          component: '/wms/transfer-application-review/raw-mat/index',
          meta: { title: '原材料-调拨审核', icon: 'project', noCache: true }
        },
        {
          name: 'RawMatReturnApplicationReview',
          path: 'return-application-review/raw-mat',
          hidden: false,
          component: '/wms/return-application-review/raw-mat/index',
          meta: { title: '原材料-退库审核', icon: 'project', noCache: true }
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
          name: 'WMSGasMaterialInventory',
          path: 'gas',
          hidden: false,
          component: '/wms/material-inventory/gas/index',
          meta: { title: '气体仓库', icon: 'project', noCache: true }
        },
        {
          name: 'WMSOutboundReview',
          path: 'outbound-application-review/raw-mat',
          hidden: false,
          component: '/wms/outbound-application-review/raw-mat/index',
          meta: { title: '原材料-出库审核', icon: 'project', noCache: true }
        },
        {
          name: 'WMSOutboundRecord',
          path: 'outbound-application-record/raw-mat',
          hidden: false,
          component: '/wms/outbound-application-record/raw-mat/index',
          meta: { title: '原材料-出库记录', icon: 'project', noCache: true }
        },
        {
          name: 'WMSPartyABorrowManage',
          path: 'party-a-borrow-manage',
          hidden: false,
          component: '/wms/party-a-borrow-manage/index',
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
      path: '/wms/freeze-manage',
      component: 'Layout',
      hidden: false,
      name: 'WMSFreezeManage',
      alwaysShow: false,
      redirect: '/wms/freeze-manage/raw-mat/index',
      meta: { title: '冻结管理', icon: 'contract', noCache: true },
      children: [
        {
          name: 'WMSRawMatFreezeList',
          path: 'raw-mat-record',
          hidden: false,
          component: '/wms/freeze-manage/raw-mat/index',
          meta: { title: '原材料-冻结列表', icon: 'project', noCache: true }
        },
        {
          name: 'WMSRawMatUnfreezeRecord',
          path: 'raw-mat-unfreeze-record',
          hidden: false,
          component: '/wms/freeze-manage/raw-mat-unfreeze-record/index',
          meta: { title: '原材料-解冻记录', icon: 'project', noCache: true }
        }
      ]
    },
    {
      path: '/wms/report',
      component: 'Layout',
      hidden: false,
      name: 'WMSReport',
      alwaysShow: false,
      redirect: '/wms/report/return-to-party-a',
      meta: { title: '报表中心', icon: 'contract', noCache: true },
      children: [
        {
          name: 'WMSReturnToPartyARecord',
          path: 'return-to-party-a',
          hidden: false,
          component: '/wms/operate-record/return-to-party-a/index',
          meta: { title: '归还甲方', icon: 'project', noCache: true }
        },
        {
          name: 'WMSPartyABuyInRecord',
          path: 'party-a-buy-in',
          hidden: false,
          component: '/wms/operate-record/party-a-buy-in/index',
          meta: { title: '甲供买入', icon: 'project', noCache: true }
        }
      ]
    },
    {
      path: '/wms/purchase-manage',
      component: 'Layout',
      hidden: false,
      name: 'WMSPurchaseOrder',
      alwaysShow: false,
      redirect: '/wms/purchase-order',
      meta: { title: '采购订单管理', icon: 'contract', noCache: true },
      children: [
        {
          name: 'PurchaseOrder',
          path: 'purchase-order',
          hidden: false,
          component: '/wms/purchase-order/index',
          meta: { title: '采购订单', icon: 'project', noCache: true }
        }
      ]
    },
    {
      path: '/wms/requisitions-manage',
      component: 'Layout',
      hidden: false,
      name: 'WMSRequisitions',
      alwaysShow: false,
      redirect: '/wms/requisitions',
      meta: { title: '申购订单管理', icon: 'contract', noCache: true },
      children: [
        {
          name: 'requisitions',
          path: 'requisitions',
          hidden: false,
          component: '/wms/requisitions/index',
          meta: { title: '申购订单', icon: 'project', noCache: true }
        }
      ]
    },
    {
      path: '/wms/logistics-manage',
      component: 'Layout',
      hidden: false,
      name: 'WMSLogisticsOrder',
      alwaysShow: false,
      redirect: '/wms/logistics-order',
      meta: { title: '物流订单管理', icon: 'contract', noCache: true },
      children: [
        {
          name: 'LogisticsOrder',
          path: 'purchase-order',
          hidden: false,
          component: '/wms/logistics-order/index',
          meta: { title: '物流订单', icon: 'project', noCache: true }
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
    },
    // {
    //   name: 'InventoryWarning',
    //   path: 'inventory-warning',
    //   hidden: false,
    //   component: '/wms-config/inventory-warning/index',
    //   meta: { title: '库存预警', icon: 'project', noCache: true }
    // },
    // {
    //   path: '/wms/prepares-materials',
    //   component: 'Layout',
    //   hidden: false,
    //   name: 'WmsPreparesMaterials',
    //   alwaysShow: false,
    //   redirect: '/wms/prepares-materials/custom',
    //   meta: { title: '备料管理', icon: 'contract', onCache: true },
    //   children: [
    //     {
    //       name: 'WmsPreparesMaterialsCustom',
    //       path: 'custom',
    //       hidden: false,
    //       component: '/wms/prepares-materials/custom/index',
    //       meta: { title: '备料定制', icon: 'project', noCache: true }
    //     },
    //     {
    //       name: 'WmsPreparesMaterialsTrack',
    //       path: 'track',
    //       hidden: false,
    //       component: '/wms/prepares-materials/track/index',
    //       meta: { title: '备料跟踪', icon: 'project', noCache: true }
    //     },
    //     {
    //       name: 'WmsFreezeMaterialManage',
    //       path: 'freeze-manage',
    //       hidden: false,
    //       component: '/wms/warehouse-management/freeze-manage',
    //       meta: { title: '冻结管理', icon: 'project', noCache: true }
    //     }
    //   ]
    // },
    {
      path: '/wms/supplier',
      component: 'Layout',
      hidden: false,
      name: 'SupplierManage',
      alwaysShow: false,
      redirect: '/wms/supplier/manage',
      meta: { title: '供应商管理', icon: 'contract', noCache: true },
      children: [
        {
          name: 'PurchaseSupplier',
          path: 'manage',
          hidden: false,
          component: '/wms/supplier/manage/index',
          meta: { title: '供应商列表', icon: 'project', noCache: true }
        }
      ]
    }
  ]
}
