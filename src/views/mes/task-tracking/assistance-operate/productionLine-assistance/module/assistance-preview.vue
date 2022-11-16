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
      <el-tag effect="plain"> 原生产组：{{ info.workshop?.name }}>{{ info.productionLine?.name }}>{{ info.groups?.name }} </el-tag>
    </template>
    <template #titleRight>
      <common-button @click="submitIt" :loading="submitLoading" size="mini" type="primary">保存</common-button>
    </template>
    <common-table :data="list" :max-height="maxHeight" style="width: 100%">
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column prop="area.name" :show-overflow-tooltip="true" label="区域" min-width="100px" align="center" />
      <el-table-column
        v-if="crud.query.taskTypeEnum !== taskTypeENUM.ARTIFACT.V"
        prop="attributeType"
        :show-overflow-tooltip="true"
        label="属性"
        width="90"
        align="center"
      >
        <template #default="{ row }">
          <el-tag :type="row.attributeType === '部件' ? 'warning' : 'success'">{{ row.attributeType }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column prop="serialNumber" :show-overflow-tooltip="true" label="编号" min-width="100px" align="center" />
      <el-table-column prop="specification" :show-overflow-tooltip="true" label="规格" min-width="100px" align="center" />
      <el-table-column prop="length" :show-overflow-tooltip="true" label="长度(mm)" width="100px" align="center" />
      <el-table-column prop="editQuantity" :show-overflow-tooltip="true" label="协同数量" width="80" align="center" />
      <el-table-column prop="groupsName" :show-overflow-tooltip="true" label="车间>生产线>生产组" min-width="150">
        <template #default="{ row }">
          <span>{{ row.workshop?.name }}>{{ row.productionLine?.name }}>{{ row.groupsName }}</span>
        </template>
      </el-table-column>
    </common-table>
  </common-dialog>
</template>

<script setup>
import { save } from '@/api/mes/task-tracking/assistance-operate/productionLine-assistance'
import { defineEmits, defineProps, ref, inject } from 'vue'
import { ElNotification } from 'element-plus'

import { taskTypeENUM } from '@enum-ms/mes'

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
    await save(_list)
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
