<template>
  <div>
    <div v-show="!lineId">
      <div class="my-code">点击生产线查看详情</div>
    </div>
    <div v-show="lineId">
      <!--表格渲染-->
      <common-table
        ref="tableRef"
        v-loading="!loaded"
        :data="list"
        :data-format="dataFormat"
        :max-height="maxHeight + 135"
        style="width: 100%"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column key="processName" prop="processName" :show-overflow-tooltip="true" label="工序名称" min-width="100px">
          <template #default="{ row }">
            <span>{{ row.processName }}</span>
          </template>
        </el-table-column>
        <el-table-column key="wageQuotaType" prop="wageQuotaType" label="计价方式" align="center" width="75px">
          <template #default="{ row }">
            <span>{{ row.wageQuotaType }}</span>
          </template>
        </el-table-column>
        <el-table-column key="organizationType" prop="organizationType" label="属性" align="center" width="75px">
          <template v-slot="scope">
            {{ teamAttributeEnum.VL[scope.row.organizationType] }}
          </template>
        </el-table-column>
        <el-table-column key="leaderName" prop="leaderName" label="组长" width="80px" />
        <el-table-column key="memberNames" prop="memberNames" :show-overflow-tooltip="true" label="组员" min-width="160px" />
      </common-table>
      <common-dialog
        title="选择班组"
        v-model="dialogVisible"
        :before-close="
          () => {
            dialogVisible = false
          }
        "
        :close-on-click-modal="false"
        width="500px"
      >
        <template #titleRight>
          <common-button :loading="submitLoading" :disabled="!selectValue?.length" size="mini" type="primary" @click="submitIt">
            保存
          </common-button>
        </template>
        <common-select
          v-model="selectValue"
          :options="productionTeamOptions"
          :type="'other'"
          multiple
          filterable
          clearable
          :dataStructure="{ key: 'id', label: 'label', value: 'id' }"
          placeholder="请选择班组"
          style="width: 100%"
        >
          <!-- <template #view="{ data: item }">
            <span>{{ item.leaderName }} | {{ item.processName }} | {{ teamAttributeEnum.VL[item.organizationType] }}</span>
          </template> -->
        </common-select>
      </common-dialog>
    </div>
  </div>
</template>

<script setup>
import { productAddTeam } from '@/api/mes/production-config/production-line'
import { defineProps, defineExpose, ref, defineEmits, watch, computed, inject } from 'vue'
import { teamAttributeEnum, wageQuotaTypeEnum } from '@enum-ms/mes'
import { cleanArray } from '@data-type/array'

import useProductionTeam from '@compos/store/use-production-team'
import { ElNotification } from 'element-plus'

const dataFormat = [['wageQuotaType', ['parse-enum', wageQuotaTypeEnum, { f: 'SL', extra: '计价' }]]]

const maxHeight = inject('maxHeight')

const { loaded, productionTeamKV, productionTeam } = useProductionTeam()
const selectValue = ref([])
const dialogVisible = ref(false)
const submitLoading = ref(false)

const emit = defineEmits(['update:modelValue', 'change'])
const props = defineProps({
  modelValue: {
    type: Array,
    default: () => []
  },
  line: {
    type: Object,
    default: () => {}
  }
})

watch(
  () => props.modelValue,
  (val) => {
    selectValue.value = val
  },
  { immediate: true }
)

watch(
  () => dialogVisible.value,
  (val) => {
    if (val) {
      selectValue.value = props.modelValue
    }
  }
)

const lineId = computed(() => {
  return props.line && props.line.id
})

const list = computed(() => cleanArray(props.modelValue.map((v) => productionTeamKV.value[v])))

const productionTeamOptions = computed(() => productionTeam.value.filter((v) => props.line?.productType & v.productType))

async function submitIt() {
  try {
    submitLoading.value = true
    await productAddTeam({
      productLineId: lineId.value,
      teamIds: selectValue.value
    })
    ElNotification({
      title: '班组绑定成功',
      type: 'success',
      duration: 2500
    })
    emit('update:modelValue', selectValue.value)
    emit('change', selectValue.value)
  } catch (error) {
    console.log(error, '绑定班组')
  } finally {
    dialogVisible.value = false
    submitLoading.value = false
  }
}

defineExpose({
  toAdd: () => (dialogVisible.value = true)
})
</script>
