<template>
  <common-drawer
    ref="drawerRef"
    append-to-body
    :close-on-click-modal="false"
    :before-close="handleClose"
    v-model="visible"
    title="规格修正"
    :wrapper-closable="false"
    size="85%"
    custom-class="spec-change-form"
  >
    <template #titleRight>
      <common-button type="success" size="mini" :loading="loading" @click="onSubmit">保存</common-button>
    </template>
    <template #content>
      <common-table
        v-if="refreshTable"
        ref="tableRef"
        border
        :data="tableList"
        :max-height="maxHeight"
        style="width: 100%"
        class="table-form"
        :stripe="false"
        return-source-data
        :cell-class-name="wrongCellMask"
        :showEmptySymbol="false"
      >
        <el-table-column label="序号" type="index" align="center" width="55" />
        <el-table-column key="monomer.name" prop="monomer.name" label="单体" show-overflow-tooltip />
        <el-table-column key="area.name" prop="area.name" label="区域" show-overflow-tooltip />
        <el-table-column key="name" prop="name" label="名称" show-overflow-tooltip />
        <el-table-column key="serialNumber" prop="serialNumber" label="编号" show-overflow-tooltip />
        <el-table-column key="newSpecPrefix" prop="newSpecPrefix" label="截面定义" show-overflow-tooltip>
          <template v-slot="scope">
            <common-select
              :key="Math.random()"
              v-model="scope.row.newSpecPrefix"
              :options="specList"
              :show-extra="scope.$index !== 0"
              type="other"
              filterable
              clearable
              :dataStructure="{ key: 'id', label: 'value', value: 'id' }"
              placeholder="截面定义"
            />
          </template>
        </el-table-column>
        <el-table-column key="newSpecSection" prop="newSpecSection" label="规格" show-overflow-tooltip>
          <template v-slot="scope">
            <el-input v-model.trim="scope.row.newSpecSection" placeholder="例如:400*200*10*12" type="text" :maxlength="30"/>
          </template>
        </el-table-column>
        <el-table-column key="quantity" prop="quantity" label="清单数" show-overflow-tooltip />
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, defineProps, defineEmits, watch, nextTick, inject } from 'vue'
import { edit } from '@/api/mes/craft-manage/artifact-specification-revise'

import { deepClone } from '@/utils/data-type'
import { createUniqueString } from '@/utils/data-type/string'
import useVisible from '@compos/use-visible'
import { ElNotification } from 'element-plus'
import useMaxHeight from '@compos/use-max-height'
import useTableValidate from '@compos/form/use-table-validate'

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  list: {
    type: Array,
    default: () => []
  }
})

const tableRef = ref()
const tableList = ref([])

const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })
const loading = ref(false)
const drawerRef = ref()
const refreshTable = ref(true)
const specList = inject('specList')

const tableRules = {
  newSpecPrefix: [{ required: true, message: '请选择截面定义', trigger: 'change' }],
  newSpecSection: [{ required: true, max: 30, message: '不能超过30个字符', trigger: 'blur' }]
}

// 同上的选项与值
const ditto = new Map([
  ['newSpecPrefix', -1],
  ['newSpecSection', '同上']
])

const { tableValidate, cleanUpData, wrongCellMask } = useTableValidate({ rules: tableRules, ditto })

watch(
  () => props.list,
  (val) => {
    if (val) {
      tableList.value = []
      initList()
    }
  },
  { deep: true, immediate: true }
)

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.spec-change-form',
    extraBox: '.el-drawer__header',
    wrapperBox: '.el-drawer__body',
    navbar: false,
    clientHRepMainH: true,
    minHeight: 300
  },
  () => drawerRef.value.loaded
)

function initList() {
  for (let i = 0; i < props.list.length; i++) {
    addRow(i)
  }
  refreshTable.value = false
  nextTick(() => {
    refreshTable.value = true
  })
}

function addRow(index) {
  const row = deepClone(props.list[index])
  row.uid = createUniqueString()
  if (ditto && tableList.value.length > 0) {
    ditto.forEach((value, key) => {
      if (!row[key]) {
        row[key] = value
      }
    })
  }
  tableList.value.push(row)
}

async function onSubmit() {
  loading.value = true
  try {
    const { validResult, dealList } = tableValidate(tableList.value)
    tableList.value = dealList
    if (validResult) {
      // 清除无用数据
      const _list = cleanUpData(deepClone(dealList))
      const submitData = _list.map(v => {
        return {
          id: v.id,
          newSpecPrefix: v.newSpecPrefix,
          newSpecSection: v.newSpecSection
        }
      })
      await edit({ list: submitData })
      handleSuccess()
    }
  } catch (e) {
    console.log('构件规格修改', e)
  } finally {
    loading.value = false
  }
}

function handleSuccess() {
  ElNotification({ title: '修正成功', type: 'success' })
  emit('success')
  handleClose()
}

</script>
<style rel="stylesheet/scss" lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
::v-deep(.el-dialog__body) {
  padding: 10px 20px;

  .el-step {
    .el-step__icon {
      width: 20px;
      height: 20px;
      font-size: 12px;
    }
    .el-step__title {
      font-size: 13px;
    }
  }
}
.tree-form {
  ::v-deep(.el-drawer__header) {
    margin-bottom: 0;
  }
}
.item-name {
  padding: 8px 16px;
  background-color: #ecf8ff;
  border-radius: 4px;
  border-left: 5px solid #50bfff;
  margin: 5px 0;
  margin-left: 5px;
  width: 150px;
}
.table-form {
  ::v-deep(.el-input__inner) {
    padding: 0;
    padding-left: 5px;
  }
  ::v-deep(.el-radio){
    display:block;
  }
  ::v-deep(.el-radio.el-radio--small){
    height:23px;
  }
}

</style>

