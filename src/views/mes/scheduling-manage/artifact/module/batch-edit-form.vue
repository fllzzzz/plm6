<template>
  <common-dialog title="批量重新分配" v-model="dialogVisible" width="1400px" :before-close="handleClose">
    <template #titleAfter>
      <el-tag size="small" effect="plain">
        <span>原产线：</span>
        <span>{{ originLabel }}</span>
      </el-tag>
    </template>
    <template #titleRight>
      <common-button :loading="submitLoading" type="primary" size="mini" @click="submitIt">确 认</common-button>
    </template>
    <common-table
      :data="list"
      return-source-data
      :span-method="spanMethod"
      :cell-class-name="wrongCellMask"
      :max-height="maxHeight"
      style="width: 100%"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column prop="monomer.name" :show-overflow-tooltip="true" label="单体" min-width="100" align="center">
        <template #default="{ row }">
          <span>{{ row.monomer?.name }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="area.name" :show-overflow-tooltip="true" label="区域" min-width="100" align="center">
        <template #default="{ row }">
          <span>{{ row.area?.name }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="serialNumber" :show-overflow-tooltip="true" label="编号" min-width="100" align="center">
        <template #default="{ row }">
          <span>{{ row.serialNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="specification" :show-overflow-tooltip="true" label="规格" min-width="120" align="center" />
      <el-table-column prop="length" :show-overflow-tooltip="true" label="长度（mm）" min-width="90" align="center" />
      <el-table-column prop="editQuantity" :show-overflow-tooltip="true" label="数量" min-width="90" align="center">
        <template #default="{ row }">
          <common-input-number
            v-model="row.editQuantity"
            :step="1"
            :min="1"
            :max="row.schedulingQuantity"
            :precision="0"
            size="small"
            controls-position="right"
            style="width: 100%"
          />
        </template>
      </el-table-column>
      <el-table-column prop="schedulingTotalNetWeight" :show-overflow-tooltip="true" label="总重（kg）" min-width="90" align="center" />
      <el-table-column prop="curGroupsId" :show-overflow-tooltip="true" label="选择生产组" min-width="150" align="center">
        <template #default="{ row }">
          <el-cascader
            v-model="row.curGroupsId"
            :options="groupsTree"
            :props="{ value: 'id', label: 'name', children: 'children', expandTrigger: 'hover', emitPath: false }"
            :show-all-levels="false"
            filterable
            clearable
            style="width: 100%"
            placeholder="请选择现生产组"
          />
        </template>
      </el-table-column>
    </common-table>
  </common-dialog>
</template>

<script setup>
import { editRecord } from '@/api/mes/scheduling-manage/artifact'
import { computed, defineEmits, defineProps, ref, inject } from 'vue'
import { ElNotification } from 'element-plus'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import useTableValidate from '@compos/form/use-table-validate'
import useSchedulingGroups from '@compos/mes/scheduling/use-scheduling-groups'

const emit = defineEmits(['update:visible', 'refresh'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  selections: {
    type: Array,
    default: () => []
  },
  structureClassId: {
    type: [Number, String, undefined]
  }
})

const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook })

const areaIdObj = inject('areaIdObj', {})
const factoryIds = inject('curFactoryIds', [])
const productType = inject('productType')
const queryParams = computed(() => {
  return {
    productType: productType,
    structureClassId: props.structureClassId
  }
})
const { groupsTree, groupsObj } = useSchedulingGroups({
  queryParams,
  factoryIds,
  disabledIds: computed(() => (props.selections?.length ? [props.selections[0].groups?.id] || [] : []))
})

const tableRules = {
  editQuantity: [{ required: true, message: '请填写数量', trigger: 'blur' }],
  curGroupsId: [{ required: true, message: '请选择生产组', trigger: 'change' }]
  // askCompleteTime: [{ required: true, message: '请选择需求完成日期', trigger: 'change' }]
}
const ditto = new Map([
  ['curGroupsId', '同上']
  // ['askCompleteTime', '同上']
])
const { tableValidate, cleanUpData, wrongCellMask } = useTableValidate({ rules: tableRules, ditto })

const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-dialog__header'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    minHeight: 300,
    navbar: false
  },
  dialogVisible
)

const originLabel = computed(() => {
  const v = props.selections?.length ? props.selections[0] : {}
  return `${v.workshop?.name}>${v.productionLine?.name}>${v.groups?.name}`
})

const submitLoading = ref(false)
const list = ref([])

// 合并单元格
function spanMethod({ row, column, rowIndex, columnIndex }) {
  if (columnIndex === 8) {
    return {
      rowspan: list.value?.length || 0,
      colspan: 1
    }
  }
}

function showHook() {
  list.value = props.selections.map((v, i) => {
    return {
      ...v,
      curGroupsId: i === 0 ? undefined : '同上',
      editQuantity: v.schedulingQuantity
    }
  })
}

async function submitIt() {
  try {
    const { validResult, dealList } = tableValidate(list.value)
    if (validResult) {
      submitLoading.value = true
      cleanUpData(dealList) // 同上赋值
      const _list = dealList.map((v) => {
        return {
          id: v.id,
          quantity: v.editQuantity,
          groupsId: v.curGroupsId,
          factoryId: areaIdObj.value[v.area?.id]?.factoryId,
          productionLineId: groupsObj[v.curGroupsId]?.productionLine?.id,
          workshopId: groupsObj[v.curGroupsId]?.workshop?.id
        }
      })
      await editRecord(_list)
      handleClose()
      ElNotification({ title: '批量重新分配成功', type: 'success', duration: 2500 })
      emit('refresh')
    }
  } catch (error) {
    console.log('批量重新分配', error)
  } finally {
    submitLoading.value = false
  }
}
</script>
