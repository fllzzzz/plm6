<template>
  <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="handleClose"
    v-model="visible"
    title="特殊零件标记"
    :wrapper-closable="false"
    size="60%"
  >
    <template #titleRight>
      <common-button type="primary" size="mini" @click="onSubmit">确认</common-button>
    </template>
    <template #content>
      <el-form ref="formRef" :model="form" size="small" label-width="140px">
        <common-table
          ref="detailRef"
          border
          :data="form.list"
          :max-height="maxHeight"
          style="width: 100%;margin-top:10px;"
          class="table-form"
          return-source-data
          :showEmptySymbol="false"
          :cell-class-name="wrongCellMask"
        >
          <el-table-column label="序号" type="index" align="center" width="50" />
          <el-table-column key="name" prop="name" label="*代表杆件类型" align="center">
            <template v-slot="scope">
              <el-input v-model.trim="scope.row.name" type="text" placeholder="代表杆件类型" maxlength="30" />
            </template>
          </el-table-column>
          <el-table-column key="specPrefix" prop="specPrefix" label="*组立号前缀(大写)" align="center">
            <template v-slot="scope">
              <el-input v-model.trim="scope.row.specPrefix" type="text" placeholder="请填写大写字母" maxlength="10" @blur="getName(scope.row,scope.$index)"/>
            </template>
          </el-table-column>
          <el-table-column key="boolSchedulingEnum" prop="boolSchedulingEnum" label="*是否有生成工序" align="center">
            <template v-slot="scope">
              <common-radio v-model="scope.row.boolSchedulingEnum" :options="whetherEnum.ENUM" type="enum" />
            </template>
          </el-table-column>
          <el-table-column key="sort" prop="sort" label="*排序" align="center">
            <template v-slot="scope">
              <el-input-number
                v-model="scope.row.sort"
                placeholder="请填写"
                type="text"
                controls-position="right"
                :min="0"
                :max="10000"
              />
            </template>
          </el-table-column>
          <el-table-column label="操作" align="center">
            <template v-slot="scope">
              <common-button size="small" class="el-icon-delete" type="danger" @click="deleteRow(scope.$index)" />
            </template>
          </el-table-column>
        </common-table>
        <div class="add-row-box">
          <common-button
            size="mini"
            icon="el-icon-circle-plus-outline"
            type="warning"
            style="margin-top: 15px"
            @click="addRow()"
            >继续添加</common-button>
        </div>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import crudApi from '@/api/config/system-config/machine-part-config'
import { ref, defineEmits, watch, defineProps, nextTick } from 'vue'
import { whetherEnum } from '@enum-ms/common'
import useVisible from '@compos/use-visible'
import useMaxHeight from '@compos/use-max-height'
import useWatchFormValidate from '@compos/form/use-watch-form-validate'
import useTableValidate from '@compos/form/use-table-validate'
import { ElMessage } from 'element-plus'

const formRef = ref()
const defaultForm = {
  list: []
}
const form = ref(JSON.parse(JSON.stringify(defaultForm)))
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

watch(
  () => props.modelValue,
  (val) => {
    if (val) {
      resetForm()
    }
  },
  { deep: true, immediate: true }
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.addForm',
  paginate: true,
  extraHeight: 40
})

const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

// 序号校验
const validateSort = (value, row) => {
  if (!value) return false
  return true
}

const tableRules = {
  name: [{ required: true, message: '请输入杆件类型', trigger: 'blur' }],
  specPrefix: [{ required: true, message: '请输入组立号前缀', trigger: 'blur' }],
  boolSchedulingEnum: [{ required: true, message: '请选择是否有生成工序', trigger: 'change' }],
  sort: [{ validator: validateSort, message: '请输入序号', trigger: 'change' }]
}

const { tableValidate, wrongCellMask } = useTableValidate({ rules: tableRules }) // 表格校验

function getName(val, index) {
  if (val.specPrefix && !/^[A-Z]+$/.test(val.specPrefix)) {
    form.value.list[index].specPrefix = undefined
  }
}

function resetForm() {
  if (formRef.value) {
    formRef.value.resetFields()
  }
  form.value.list = []
  if (formRef.value) {
    nextTick(() => {
      formRef.value.clearValidate()
    })
  }
  useWatchFormValidate(formRef, form)
}

function addRow() {
  form.value.list.push({
    name: undefined,
    boolSchedulingEnum: undefined,
    specPrefix: undefined,
    dataIndex: form.value.list.length + 1
  })
}

function deleteRow(index) {
  form.value.list.splice(index, 1)
}

async function onSubmit() {
  try {
    const { validResult, dealList } = tableValidate(form.value.list)
    if (validResult) {
      form.value.list = dealList
    } else {
      return validResult
    }
    await crudApi.add(form.value.list)
    ElMessage.success('添加成功')
    emit('success')
    handleClose()
  } catch (e) {
    console.log('添加特殊零件标记', e)
  }
}
</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
.process-container {
  display: flex;
  flex-direction: row;
  justify-content: flex-start;
  align-items: flex-end;
  .process-box {
    display: flex;
    flex-direction: column;
    justify-content: flex-start;
    align-items: flex-start;
    .process-drawer {
      display: flex;
      flex-direction: row;
      justify-content: flex-start;
      align-items: center;
      margin-bottom: 10px;
    }
  }
}
.add-row-box{
  text-align: center;
}
</style>
