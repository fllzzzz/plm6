<template>
  <common-dialog
    custom-class="assistance-preview"
    append-to-body
    :close-on-click-modal="false"
    title="产线协同预览"
    v-model="dialogVisible"
    width="1100px"
    :before-close="handleClose"
  >
    <template #titleAfter>
      <el-tag effect="plain" v-if="crud.query.taskTypeEnum !== bridgeTaskTypeEnum.MACHINE_PART.V">
        原生产组：{{ info.workshop?.name }}>{{ info.productionLine?.name }}>{{ info.groups?.name }}
      </el-tag>
    </template>
    <template #titleRight>
      <common-button @click="submitIt" :loading="submitLoading" size="mini" type="primary">保存</common-button>
    </template>
    <common-table :data="list" :data-format="dataFormat" :max-height="maxHeight" style="width: 100%">
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="crud.query.taskTypeEnum !== bridgeTaskTypeEnum.MACHINE_PART.V"
        prop="area.name"
        :show-overflow-tooltip="true"
        label="区域"
        min-width="100px"
        align="center"
      />
      <el-table-column
        v-if="crud.query.taskTypeEnum === bridgeTaskTypeEnum.CELL.V"
        prop="attributeType"
        :show-overflow-tooltip="true"
        label="属性"
        width="90"
        align="center"
      >
        <template #default="{ row }">
          <el-tag :type="row.attributeType === '单元件' ? 'warning' : 'success'">{{ row.attributeType }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column
        v-if="crud.query.taskTypeEnum === bridgeTaskTypeEnum.MACHINE_PART.V"
        prop="cutNumber"
        :show-overflow-tooltip="true"
        label="切割指令号"
        min-width="100px"
        align="center"
      />
      <el-table-column
        v-if="crud.query.taskTypeEnum !== bridgeTaskTypeEnum.MACHINE_PART.V"
        prop="serialNumber"
        :show-overflow-tooltip="true"
        label="编号"
        min-width="100px"
        align="center"
      />
      <el-table-column prop="specification" :show-overflow-tooltip="true" label="规格" min-width="100px" align="center" />
      <el-table-column
        v-if="crud.query.taskTypeEnum !== bridgeTaskTypeEnum.MACHINE_PART.V"
        prop="length"
        :show-overflow-tooltip="true"
        label="长度(mm)"
        width="100px"
        align="center"
      />
      <el-table-column prop="editQuantity" :show-overflow-tooltip="true" label="协同数量" width="80" align="center" />
      <el-table-column
        v-if="crud.query.taskTypeEnum === bridgeTaskTypeEnum.MACHINE_PART.V"
        align="center"
        prop="askCompleteTime"
        :show-overflow-tooltip="true"
        label="计划完成日期"
        width="120px"
      />
      <el-table-column
        v-if="crud.query.taskTypeEnum === bridgeTaskTypeEnum.MACHINE_PART.V"
        prop="group.name"
        :show-overflow-tooltip="true"
        label="原生产组"
        min-width="160px"
      >
        <template #default="{ row }">
          <span>{{ row.workshop?.name }}>{{ row.productionLine?.name }}>{{ row.groups?.name }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="groupsName" :show-overflow-tooltip="true" label="协同产组" min-width="150">
        <template #default="{ row }">
          <span>{{ row.assistance?.workshop?.name }}>{{ row.assistance?.productionLine?.name }}>{{ row.assistance?.groupsName }}</span>
        </template>
      </el-table-column>
    </common-table>
  </common-dialog>
</template>

<script setup>
import { save, saveNest } from '@/api/bridge/bridge-task-tracking/assistance-operate/productionLine-assistance'
import { defineEmits, defineProps, ref, inject } from 'vue'
import { ElNotification } from 'element-plus'

import { bridgeTaskTypeEnum } from '@enum-ms/bridge'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

const emit = defineEmits(['update:visible', 'success'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  info: {
    type: Object,
    default: () => {}
  },
  list: {
    type: Array,
    default: () => []
  }
})

const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible' })
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.assistance-preview',
    extraBox: ['.el-dialog__header'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    minHeight: 300,
    navbar: false
  },
  dialogVisible
)

const dataFormat = ref([['askCompleteTime', ['parse-time', '{y}-{m}-{d}']]])
const crud = inject('crud')
const submitLoading = ref(false)

async function submitIt() {
  try {
    submitLoading.value = true
    const _list = props.list.map((v) => {
      return {
        id: v.id,
        quantity: v.editQuantity,
        groupsId: v.groupsId
      }
    })
    if (crud.query.taskTypeEnum === bridgeTaskTypeEnum.MACHINE_PART.V) {
      await saveNest(_list)
    } else {
      await save(_list)
    }
    ElNotification({
      title: '产线协同保存成功',
      type: 'success',
      duration: 2500
    })
    handleClose()
    emit('success')
  } catch (error) {
    console.log('保存产线协同报错', error)
  } finally {
    submitLoading.value = false
  }
}
</script>
