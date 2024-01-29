<template>
  <common-dialog v-model="visible" :before-close="handleClose" title="新增合同" width="55%">
    <template #titleRight>
      <common-button type="success" icon="el-icon-plus" size="mini" @click="addColumn" />
      <common-button type="primary" size="mini" @click="verify">确认</common-button>
    </template>
    <common-table :data="tableData" returnSourceData>
      <el-table-column label="序号" align="center" width="100" type="index"></el-table-column>
      <el-table-column label="合同编号" align="center">
        <template #default="{row}">
          <el-input v-model="row.serialNumber" />
        </template>
      </el-table-column>
      <el-table-column label="购买方" align="center">
        <template #default="{row}">
          <el-input v-model="row.purchaser" />
        </template>
      </el-table-column>
      <el-table-column label="操作" align="center" width="150">
        <template #default="{$index}">
          <common-button icon="el-icon-delete" type="danger" @click="delColumn($index)" />
        </template>
      </el-table-column>
    </common-table>
  </common-dialog>
</template>
<script setup>
import { defineProps, defineEmits, ref } from 'vue'
import { addScrapContractList } from '@/api/contract/scrap-ledger'
import useVisible from '@compos/use-visible'

const props = defineProps({
  modelValue: {
    type: Boolean,
    default: false
  }
})

const emit = defineEmits(['update:modelValue', 'success'])

const { visible, handleClose } = useVisible({ props, emit, showHook: fetchData })

const tableData = ref([
  {
    purchaser: '',
    serialNumber: ''
  }
])

function fetchData() {
  tableData.value = [
    {
      purchaser: '',
      serialNumber: ''
    }
  ]
}

async function verify() {
  const _list = []
  tableData.value.forEach(v => {
    if (v.purchaser && v.serialNumber) {
      _list.push(v)
    }
  })
  try {
    await addScrapContractList(_list)
    handleClose()
    emit('success')
  } catch (error) {
    console.log(error, '合同配置列表新增失败')
  }
}

function addColumn() {
  tableData.value.push(
    {
      purchaser: '',
      serialNumber: ''
    }
  )
}

function delColumn(index) {
  tableData.value.splice(index, 1)
}

</script>
