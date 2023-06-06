<template>
  <div class="detail-container">
    <div style="margin-bottom: 10px; min-height: 28px" class="tag-div">
      <div style="float: left">
        <el-tag
size="medium"
v-if="props.currentRow.classificationId === null"
          >{{ `${bridgeComponentTypeEnum.VL[productType]}分类：` }}未知类型</el-tag
        >
        <el-tag size="medium" v-else>{{ `${bridgeComponentTypeEnum.VL[productType]}分类：${currentRow.classificationName}` }}</el-tag>
        <template v-if="productType === bridgeComponentTypeEnum.MACHINE_PART.V">
          <template v-if="isNotBlank(currentRow.specifications)">
            <common-select
              v-model="specification"
              :options="specOption"
              type="other"
              size="mini"
              :dataStructure="{ key: 'label', label: 'label', value: 'value' }"
              clearable
              placeholder="请选择厚度"
              style="width: 200px; margin: 0 10px"
              @change="fetchDetail"
            />
            <common-radio-button
              v-model="boolHaveHole"
              :options="hasHoleEnum.ENUM"
              showOptionAll
              size="mini"
              :optionAllValue="undefined"
              type="enum"
              style="vertical-align: middle"
              @change="fetchDetail"
            />
          </template>
          <template v-else>
            <el-input
              v-model.trim="specification"
              type="text"
              placeholder="请输入规格"
              style="width: 200px; margin: 0 10px"
              @blur="fetchDetail"
            />
          </template>
        </template>
      </div>
      <div style="float: right">
        <print-table :api-key="apiKey" :params="{ ...params }" size="mini" type="warning" />
      </div>
    </div>
    <common-table :data="list" v-loading="tableLoading" :max-height="maxHeight - 140">
      <el-table-column key="serialNumber" prop="serialNumber" label="编号" align="center" :show-overflow-tooltip="true" />
      <el-table-column key="specification" prop="specification" label="规格" align="center" min-width="120" :show-overflow-tooltip="true" />
      <el-table-column key="length" prop="length" label="长度" align="center" :show-overflow-tooltip="true" />
      <el-table-column key="quantity" prop="quantity" label="数量" align="center" :show-overflow-tooltip="true" />
      <el-table-column key="netWeight" prop="netWeight" label="单量（kg）" align="center" :show-overflow-tooltip="true">
        <template v-slot="scope">
          <span v-if="scope.row.netWeight">{{ toThousand(scope.row.netWeight, DP.COM_WT__KG) }}</span>
          <span v-else>-</span>
        </template>
      </el-table-column>
    </common-table>
  </div>
</template>

<script setup>
import { ref, defineProps, watch, computed } from 'vue'
import { boxDetail, elementDetail, machinePartDetail } from '@/api/bridge/production-order-manage/production-order'

import useMaxHeight from '@compos/use-max-height'
import { DP } from '@/settings/config'
import { toThousand } from '@/utils/data-type/number'
import { isNotBlank } from '@data-type/index'
import { hasHoleEnum } from '@enum-ms/mes'
import { bridgeComponentTypeEnum } from '@enum-ms/bridge'

const props = defineProps({
  currentRow: {
    type: Object,
    default: () => {}
  },
  productType: {
    type: [Number, String],
    default: undefined
  }
})

const list = ref([])
const tableLoading = ref(false)
const specOption = ref([])
const specification = ref()
const boolHaveHole = ref()
const params = computed(() => {
  return {
    projectId: props.currentRow.projectId,
    material: props.currentRow.material,
    classificationName: props.currentRow.classificationName
  }
})

const apiKey = computed(() => {
  return props.productType === bridgeComponentTypeEnum.MACHINE_PART.V
    ? 'bridgeMachinePartClassList'
    : props.productType === bridgeComponentTypeEnum.BOX.V
      ? 'bridgeBoxClassList'
      : 'bridgeElementClassList'
})

const { maxHeight } = useMaxHeight({ extraBox: '.tag-div', wrapperBox: ['.app-container', '.detail-container'] })
watch(
  () => params.value,
  (val) => {
    if (val) {
      specOption.value = []
      boolHaveHole.value = undefined
      specification.value = undefined
      if (props.productType === bridgeComponentTypeEnum.MACHINE_PART.V) {
        if (isNotBlank(props.currentRow.specifications)) {
          for (let i = 0; i < props.currentRow.specifications.length; i++) {
            specOption.value.push({
              label: 'PL=' + props.currentRow.specifications[i],
              value: props.currentRow.specifications[i]
            })
          }
        }
      }
      fetchDetail()
    }
  },
  { deep: true, immediate: true }
)

async function fetchDetail() {
  list.value = []
  if (!props.currentRow.projectId) {
    return
  }
  tableLoading.value = true
  const query =
    props.productType === bridgeComponentTypeEnum.MACHINE_PART.V
      ? {
        ...params.value,
        boolHaveHole: boolHaveHole.value,
        specification: specification.value
      }
      : { ...params.value }
  try {
    const api =
      props.productType === bridgeComponentTypeEnum.BOX.V
        ? boxDetail
        : props.productType === bridgeComponentTypeEnum.CELL.V
          ? elementDetail
          : machinePartDetail
    const { content } = await api({ ...query })
    list.value = content
    tableLoading.value = false
  } catch (error) {
    console.log('获取详情失败', error)
    tableLoading.value = false
  }
}
</script>
