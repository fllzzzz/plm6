<template>
  <div>
    <div v-show="!lineId">
      <div class="my-code">点击生产线查看详情</div>
    </div>
    <div v-show="lineId">
      <!--表格渲染-->
      <common-table ref="tableRef" v-loading="!loaded" returnSourceData :data="list" :max-height="maxHeight + 42 + 55" style="width: 100%">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column key="processName" prop="processName" :show-overflow-tooltip="true" label="工序名称" width="120px" />
        <el-table-column key="inspectorNames" prop="inspectorNames" :show-overflow-tooltip="true" label="质检" min-width="160px" />
      </common-table>
      <common-dialog
        title="选择质检班组"
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
          :options="inspectionTeam"
          :type="'other'"
          multiple
          filterable
          clearable
          :dataStructure="{ key: 'id', label: 'processName', value: 'id' }"
          placeholder="请选择质检班组"
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
import { productAddInspectionTeam } from '@/api/mes/production-config/production-line'
import { defineExpose, defineEmits, ref, defineProps, watch, computed, inject } from 'vue'
import useInspectionTeam from '@compos/store/use-inspection-team'
import { ElNotification } from 'element-plus'
import { cleanArray } from '@data-type/array'

const maxHeight = inject('maxHeight')

const { loaded, inspectionTeamKV, inspectionTeam } = useInspectionTeam()
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

const list = computed(() => cleanArray(props.modelValue.map((v) => inspectionTeamKV.value[v])))

async function submitIt() {
  try {
    submitLoading.value = true
    await productAddInspectionTeam({
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
