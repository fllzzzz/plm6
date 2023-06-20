<template>
  <common-dialog title="新增零件作废" v-model="dialogVisible" :show-close="false" :before-close="handleClose">
    <template #titleRight>
      <el-button type="primary" size="mini" @click="handleSave">确认</el-button>
      <el-button size="mini" @click="handleClose">取消</el-button>
    </template>
    <template #titleAfter>
      <el-tag size="medium" effect="plain" style="margin-right: 5px"> 零件编号：{{ info.serialNumber }} </el-tag>
      <el-tag size="medium" effect="plain"> 零件规格：{{ info.specification }} </el-tag>
    </template>
    <common-table
      return-source-data
      v-loading="tableLoading"
      :data="tableData"
      :max-height="maxHeight"
      style="width: 100%"
      @selection-change="handleSelectChange"
    >
      <el-table-column type="selection" width="55" align="center" />
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        key="serialNumber"
        prop="serialNumber"
        :show-overflow-tooltip="true"
        label="构件编号"
        min-width="120"
        align="center"
      />
      <el-table-column key="quantity" prop="quantity" :show-overflow-tooltip="true" label="数量" min-width="100" align="center">
        <template #default="{ row }">
          <common-input-number
            v-model="row.quantity"
            :step="1"
            :min="1"
            :max="row.originQuantity"
            :precision="0"
            size="mini"
            style="width: 100%"
            class="input-border-none"
          />
        </template>
      </el-table-column>
      <el-table-column :show-overflow-tooltip="true" label="构件班组" min-width="200" align="center">
        <template #default="{ row }">
          <span>{{ row.workshop?.name }}>{{ row.productionLine?.name }}>{{ row.groups?.name }}</span>
        </template>
      </el-table-column>
      <el-table-column
        key="askCompleteTime"
        prop="askCompleteTime"
        :show-overflow-tooltip="true"
        label="要求完成日期"
        width="170"
        align="center"
      >
        <template #default="{ row }">
          <span>{{ parseTime(row.askCompleteTime, '{y}-{m}-{d}') }}</span>
        </template>
      </el-table-column>
    </common-table>
  </common-dialog>
</template>

<script setup>
import { getNeedDetail, addUnProduct } from '@/api/mes/scheduling-manage/machine-part'
import { defineEmits, defineProps, ref } from 'vue'

import { parseTime } from '@/utils/date'
import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import { ElMessage, ElMessageBox, ElNotification } from 'element-plus'

const emit = defineEmits(['update:visible', 'close', 'success'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  info: {
    type: Object,
    default: () => ({})
  }
})

const { visible: dialogVisible, handleClose } = useVisible({
  emit,
  props,
  field: 'visible',
  showHook: fetchData,
  closeHook: () => {
    emit('close')
  }
})
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

const tableLoading = ref(false)
const tableData = ref([])
const selections = ref([])

async function fetchData() {
  tableLoading.value = true
  try {
    const ids = props.info.needMachinePartLinkList?.map((v) => v.id)
    const { content } = await getNeedDetail({ ids })
    tableData.value = content.map((v) => {
      v.originQuantity = v.quantity
      return v
    })
  } catch (error) {
    console.error(error)
  }
  tableLoading.value = false
}

function handleSelectChange(val) {
  selections.value = val
}

async function handleSave() {
  if (selections.value.length === 0) {
    ElMessage.warning('请选择数据')
    return
  }
  const data = selections.value.map((v) => {
    return {
      needId: v.id,
      quantity: v.quantity,
      serialNumber: v.serialNumber
    }
  })
  try {
    await ElMessageBox.confirm(
      `本次作废\n零件数量：${data.reduce((total, cur) => total + cur.quantity, 0)}\n关联构件: ${data
        .map((v) => v.serialNumber)
        .join('、')}`,
      '提示',
      {
        confirmButtonText: '确定',
        cancelButtonText: '取消'
      }
    )
    await addUnProduct(data)
    ElNotification({
      title: '作废成功',
      type: 'success',
      duration: 2000
    })
    emit('update:visible', false)
    handleClose()
    emit('success')
  } catch (error) {
    console.error(error)
  }
}
</script>
