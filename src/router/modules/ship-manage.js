export default {
  id: 9578,
  name: '发运管理',
  children: [
    {
      path: '/ship-manage',
      component: 'Layout',
      hidden: false,
      name: 'ShipManagePackAndShip',
      alwaysShow: false,
      redirect: '/ship-manage/pack-and-ship/ship-summary',
      meta: {
        title: '发运管理',
        icon: 'project',
        noCache: true
      },
      children: [
        {
          name: 'ShipManageShipSummary',
          path: 'ship-summary',
          component: '',
          hidden: false,
          alwaysShow: false,
          redirect: '/ship-manage/pack-and-ship/ship-summary/mes-ship-summary',
          meta: {
            title: '发运统计',
            icon: 'project',
            noCache: true
          },
          children: [
            {
              name: 'MesShipManageShipSummary',
              path: 'mes-ship-summary',
              hidden: false,
              component: '/ship-manage/pack-and-ship/ship-summary/mes-ship-summary/index',
              meta: {
                title: '发运统计-建钢',
                icon: 'project',
                noCache: true
              }
            },
            {
              name: 'EnclosureShipManageShipSummary',
              path: 'enclosure-ship-summary',
              hidden: false,
              component: '/ship-manage/pack-and-ship/ship-summary/enclosure-ship-summary/index',
              meta: {
                title: '发运统计-围护',
                icon: 'project',
                noCache: true
              }
            },
            {
              name: 'BridgeShipManageShipSummary',
              path: 'bridge-ship-summary',
              hidden: false,
              component: '/ship-manage/pack-and-ship/ship-summary/bridge-ship-summary/index',
              meta: {
                title: '发运统计-桥梁',
                icon: 'project',
                noCache: true
              }
            }
          ]
        },
        {
          name: 'ShipManageLogisticsList',
          path: 'logistics-list',
          hidden: false,
          component: '/ship-manage/pack-and-ship/logistics-list/index',
          meta: {
            title: '物流记录',
            icon: 'project',
            noCache: true
          }
        },
        {
          path: 'pack-manage',
          component: '',
          hidden: false,
          name: 'ShipManagePackManage',
          alwaysShow: false,
          redirect: '/ship-manage/pack-and-ship/pack-manage/manual-pack',
          meta: {
            title: '打包管理',
            icon: 'project',
            noCache: true
          },
          children: [
            {
              name: 'ShipManageManualPack',
              path: 'manual-pack',
              hidden: false,
              component: '/ship-manage/pack-and-ship/manual-pack/index',
              meta: {
                title: '打包操作',
                icon: 'project',
                noCache: true
              }
            },
            {
              name: 'ShipManagePackList',
              path: 'pack-list',
              hidden: false,
              component: '/ship-manage/pack-and-ship/pack-list/index',
              meta: {
                title: '打包记录',
                icon: 'project',
                noCache: true
              }
            }
          ]
        },
        {
          name: 'ShipManageShipList',
          path: 'ship-list',
          hidden: false,
          component: '/ship-manage/pack-and-ship/ship-list/index',
          meta: {
            title: '发运记录',
            icon: 'project',
            noCache: true
          }
        },
        {
          name: 'ShipManageReceiptStatus',
          path: 'receipt-status',
          hidden: false,
          component: '/ship-manage/pack-and-ship/receipt-status/index',
          meta: {
            title: '收货状态',
            icon: 'project',
            noCache: true
          }
        },

        {
          name: 'ShipManageShipAudit',
          path: 'ship-audit',
          hidden: false,
          component: '/ship-manage/pack-and-ship/ship-audit/index',
          meta: {
            title: '发运审核',
            icon: 'project',
            noCache: true
          }
        },
        {
          name: 'ShipManageProductSendReceiveStorage',
          path: 'product-send-receive-storage',
          hidden: false,
          alwaysShow: false,
          component: '',
          redirect: '/ship-manage/pack-and-ship/product-send-receive-storage',
          meta: {
            title: '制品入发存',
            icon: 'project',
            noCache: true
          },
          children: [
            {
              name: 'StructureProductSendReceiveStorage',
              path: 'structure',
              hidden: false,
              component: '/ship-manage/pack-and-ship/product-send-receive-storage/structure/index',
              meta: {
                title: '结构制品',
                icon: 'project',
                noCache: true
              }
            },
            {
              name: 'enclosureProductSendReceiveStorage',
              path: 'enclosure',
              hidden: false,
              component: '/ship-manage/pack-and-ship/product-send-receive-storage/enclosure/index',
              meta: {
                title: '围护制品',
                icon: 'project',
                noCache: true
              }
            },
            {
              name: 'boxProductSendReceiveStorage',
              path: 'box',
              hidden: false,
              component: '/ship-manage/pack-and-ship/product-send-receive-storage/box/index',
              meta: {
                title: '分段制品',
                icon: 'project',
                noCache: true
              }
            }
          ]
        }
      ]
    }
  ]
}

