<template>
  <common-drawer ref="drawerRef" title="项目工价审核" v-model="drawerVisible" direction="rtl" :before-close="handleClose" size="100%">
    <template #titleRight> </template>
    <template #content>
      <common-table row-key="rowId" v-loading="tableLoading" :data="list" :max-height="maxHeight" style="width: 100%">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <belonging-info-columns showMonomer showArea showTeam />
        <el-table-column prop="serialNumber" :show-overflow-tooltip="true" label="编号" min-width="140px">
          <template v-slot="scope">
            <span>{{ scope.row.serialNumber }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="specification" :show-overflow-tooltip="true" label="规格" min-width="140px">
          <template v-slot="scope">
            <span>{{ scope.row.specification }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="taskQuantity" label="任务数量" align="center" width="100px" />
        <el-table-column prop="taskMete" :label="`任务量(${unitObj.unit})`" align="center" width="100px">
          <template #default="{ row }">
            <span>{{ row.taskMete }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="checkMete" :label="`核算量(${checkUnitObj.UNIT})`" align="center" width="100px">
          <template #default="{ row }">
            <span>{{ row.checkMete }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="unitPrice" align="center" width="115px" label="单价(元)">
          <template #default="{ row }">
            <span v-to-fixed="{ k: 'YUAN', val: row.unitPrice }"></span>
          </template>
        </el-table-column>
        <el-table-column prop="totalAmount" label="总价(元)" align="center" width="100px">
          <template #default="{ row }">
            <span v-to-fixed="{ k: 'YUAN', val: row.totalAmount }"></span>
          </template>
        </el-table-column>
        <el-table-column prop="userName" show-overflow-tooltip label="操作人">
          <template v-slot="scope">
            <span>{{ scope.row.userName }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="createTime" show-overflow-tooltip label="日期" align="center">
          <template v-slot="scope">
            <span v-parse-time="{ val: scope.row.createTime, fmt: '{y}-{m}-{d}' }" />
          </template>
        </el-table-column>
        <el-table-column prop="auditName" show-overflow-tooltip label="审核人">
          <template v-slot="scope">
            <span>{{ scope.row.auditName }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="auditTime" show-overflow-tooltip label="审核日期" align="center">
          <template v-slot="scope">
            <span v-parse-time="{ val: scope.row.auditTime, fmt: '{y}-{m}-{d}' }" />
          </template>
        </el-table-column>
        <el-table-column prop="auditStatus" show-overflow-tooltip label="操作" width="170px" align="center">
          <template v-slot="scope">
            <span v-if="scope.row.auditStatus === reviewStatusEnum.UNREVIEWED.V">
              <common-button
                size="mini"
                type="success"
                :disabled="scope.row.auditLoading"
                @click="auditIt(scope.row, reviewStatusEnum.PASS.V)"
                >同意</common-button
              >
              <common-button
                size="mini"
                type="danger"
                :disabled="scope.row.auditLoading"
                @click="auditIt(scope.row, reviewStatusEnum.REFUSE.V)"
                >拒绝</common-button
              >
            </span>
            <el-tag v-else :type="reviewStatusEnum.V[scope.row.auditStatus].TAG">
              {{ reviewStatusEnum.VL[scope.row.auditStatus] }}
            </el-tag>
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { checkList, check } from '@/api/mes/team-report/off-staff'
import { defineProps, defineEmits, inject, ref, watch } from 'vue'

import { reviewStatusEnum } from '@enum-ms/common'
import { componentTypeEnum } from '@enum-ms/mes'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import useWageQuotaMeteConvert from '@compos/mes/use-wage-quota-mete-convert'
import useProductMeteConvert from '@compos/mes/use-product-mete-convert'
import belongingInfoColumns from '@comp-mes/table-columns/belonging-info-columns'

const drawerRef = ref()
const emit = defineEmits(['update:visible', 'refresh'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible' })

// 高度
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)
const query = inject('query')
const unitObj = inject('unitObj')
const checkUnitObj = inject('checkUnitObj')

watch(
  () => props.visible,
  (visible) => {
    if (visible) {
      fetchList()
    }
  },
  { immediate: true }
)
const tableLoading = ref(false)
const list = ref([])

const dataPath = {
  [componentTypeEnum.ARTIFACT.V]: 'artifact',
  [componentTypeEnum.ASSEMBLE.V]: 'assemble',
  [componentTypeEnum.MACHINE_PART.V]: 'machinePart',
  [componentTypeEnum.ENCLOSURE.V]: 'enclosure'
}

async function fetchList() {
  try {
    tableLoading.value = true
    const data = await checkList(query)
    list.value = data[dataPath[query.productType]].map((v, i) => {
      v.rowId = i + '' + Math.random()
      v.auditLoading = false
      v.unitPrice = v.wage || 0
      v.originUnitPrice = v.unitPrice
      v.taskMete = useProductMeteConvert({
        productType: query.productType,
        length: { num: v.taskLength, to: unitObj.value.unit, dp: unitObj.value.dp },
        weight: { num: v.taskNetWeight, to: unitObj.value.unit, dp: unitObj.value.dp }
      })
      v.checkMete = useWageQuotaMeteConvert({
        length: v.mate,
        weight: v.mate,
        surfaceArea: v.mate,
        wageQuotaType: v.wageQuotaType
      }).convertMete
      v.totalAmount = v.checkMete * v.unitPrice
      return v
    })
  } catch (error) {
    console.log('获取工价调整审核列表失败', error)
  } finally {
    tableLoading.value = false
  }
}

async function auditIt(row, status) {
  try {
    row.auditLoading = true
    await check({
      id: row.id,
      status
    })
    fetchList()
    emit('refresh')
  } catch (error) {
    console.log('工价调整审核失败', error)
  } finally {
    row.auditLoading = false
  }
}
</script>
