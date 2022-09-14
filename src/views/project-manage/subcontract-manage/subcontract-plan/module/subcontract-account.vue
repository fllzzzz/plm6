<template>
  <common-drawer
    ref="drawerRef"
    title="分包账号"
    :close-on-click-modal="false"
    v-model="visible"
    direction="rtl"
    :before-close="handleClose"
    custom-class="invoice-record"
    size="80%"
  >
    <template #content>
      <common-table :data="list" v-loading="tableLoading" :max-height="maxHeight" :span-method="objectSpanMethod" :stripe="false">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column prop="supplierName" label="分包公司" align="center" show-overflow-tooltip />
        <el-table-column prop="areaName" label="负责区域" align="center" min-width="120" show-overflow-tooltip />
        <el-table-column prop="quantity" label="分包量" align="center" show-overflow-tooltip>
          <template #default="{ row }">
            <span style="margin-right:2px;">{{row.quantity}}{{row.unit}}</span>|<span style="margin-left:2px;">{{row.mete}}{{row.accountUnit}}</span>
          </template>
        </el-table-column>
        <el-table-column prop="username" label="登陆账号" align="center" show-overflow-tooltip />
        <el-table-column prop="password" label="登陆密码" align="center" show-overflow-tooltip />
        <el-table-column prop="boolRelationStatus" label="状态" align="center" show-overflow-tooltip width="80">
          <template #default="{ row }">
            <el-tag :type="row.boolRelationStatus===relationStatusEnum.FALSE.V?'danger':''">{{relationStatusEnum.VL[row.boolRelationStatus]}}</el-tag>
          </template>
        </el-table-column>
        <el-table-column prop="boolSettlementStatus" label="结算状态" align="center" show-overflow-tooltip width="80">
          <template #default="{ row }">
            <el-tag :type="row.boolSettlementStatus?'success':''">{{row.boolSettlementStatus?'已结算':'未结算'}}</el-tag>
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { supplierAccountList } from '@/api/project-manage/subcontract-plan'
import { ref, defineEmits, defineProps, watch } from 'vue'

import { TechnologyTypeAllEnum } from '@enum-ms/contract'
import { relationStatusEnum } from '@enum-ms/project'

import useVisible from '@/composables/use-visible'
import useMaxHeight from '@compos/use-max-height'

const emit = defineEmits(['update:modelValue'])

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  query: {
    type: Object,
    default: () => {}
  },
  permission: {
    type: Object,
    default: () => {}
  }
})

const { visible, handleClose } = useVisible({ emit, props })

watch(
  visible,
  (val) => {
    if (val) {
      fetchList()
    }
  }
)

const list = ref([])
const drawerRef = ref()
const tableLoading = ref(false)

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.invoice-record',
    extraBox: '.el-drawer__header',
    wrapperBox: '.el-drawer__body',
    paginate: true,
    minHeight: 300,
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

function objectSpanMethod({ row, column, rowIndex, columnIndex }) {
  if (columnIndex === 1 || columnIndex === 4 || columnIndex === 5) {
    if (row.rowNumSpan) {
      return {
        rowspan: row.rowNumSpan,
        colspan: 1
      }
    } else {
      return {
        rowspan: 0,
        colspan: 1
      }
    }
  }
}

// 获取分包商登录账号
async function fetchList() {
  if (!props.query.projectId) {
    return false
  }
  const _list = []
  tableLoading.value = true
  try {
    const { content = [] } = await supplierAccountList({ ...props.query })
    content.map(v => {
      if (v.projectAreaDTOList.length) {
        v.projectAreaDTOList.map((k, index) => {
          _list.push({
            ...k,
            supplierName: v.supplierName,
            username: v.username,
            password: v.password,
            areaName: k.name + '-' + k.axis,
            unit: k.productType === TechnologyTypeAllEnum.STRUCTURE.V ? '件' : '张',
            accountUnit: k.productType === TechnologyTypeAllEnum.STRUCTURE.V ? 't' : 'm',
            rowNumSpan: index === 0 ? v.projectAreaDTOList.length : 0
          })
        })
      }
    })
  } catch (error) {
    console.log('获取分包账号失败', error)
  } finally {
    list.value = _list
    tableLoading.value = false
  }
}
</script>
