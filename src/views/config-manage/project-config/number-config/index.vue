<template>
  <div class="app-container" @click.self.stop="save">
    <!--工具栏-->
    <div v-if="checkPermission(permission.edit)" class="head-container" @click.stop="save">
      <template v-if="editAll">
        <common-button type="primary" size="mini" @click.stop="closeEditAll">取消</common-button>
        <common-button size="mini" type="success" :loading="submitLoading" @click="saveAll">保存</common-button>
      </template>
      <common-button v-else type="primary" size="mini" @click="openEditAll">编辑</common-button>
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :max-height="maxHeight"
      style="width: 100%"
      :cell-class-name="changedCellMask"
      @row-click="save"
      @row-dblclick="dblclick"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column key="type" prop="type" label="名称" align="center">
        <template v-slot="scope">
          {{ numberTypeEnum.VL[scope.row.type] }}
        </template>
      </el-table-column>
      <el-table-column key="code" prop="code" :show-overflow-tooltip="true" label="代号" align="center">
        <template v-slot="scope">
          <div v-if="!scope.row.isEdit">{{ scope.row.code  }}</div>
          <el-input
            v-else
            v-model.trim="scope.row.code"
            clearable
            type="text"
            size="small"
            maxlength="10"
            placeholder="请填写代号"
            style="width: 100%;"
            @keydown.enter="save"
          />
        </template>
      </el-table-column>
      <el-table-column key="number" prop="number" :show-overflow-tooltip="true" label="流水号" align="center">
        <template v-slot="scope">
          <div v-if="!scope.row.isEdit">{{ scope.row.number }}</div>
          <el-input-number
            v-else
            v-model="scope.row.number"
            :step="1"
            :min="1"
            :max="10"
            size="small"
            style="width: 100%;"
            @keydown.enter="save"
          />
        </template>
      </el-table-column>
    </common-table>
  </div>
</template>

<script setup>
// 系统字段添加标识
import crudApi from '@/api/config/project-config/number-config'
import { ref } from 'vue'

import { numberTypeEnum } from '@enum-ms/config'
import { debounce } from '@/utils'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import useTableChange from '@compos/form/use-table-change'
import checkPermission from '@/utils/system/check-permission'
import { numberConfigPM as permission } from '@/page-permission/config'

const sourceMap = new Map([
  ['code', 'sourceCode'],
  ['number', 'sourceNumber']
])

const editRow = ref({})
const editAll = ref(false)
const submitLoading = ref(false)

const tableRef = ref()
const { CRUD, crud } = useCRUD(
  {
    title: '编号配置',
    formStore: false,
    sort: ['id.desc'],
    permission: { ...permission },
    crudApi: { ...crudApi }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })
const { changedCellMask } = useTableChange({ fieldMap: sourceMap })

CRUD.HOOK.handleRefresh = (crud, { data: { content = [] }}) => {
  content.forEach((v) => {
    v.sourceCode = v.code
    v.sourceNumber = v.number
    v.isEdit = false
  })
}

// 双击行编辑
function dblclick(row) {
  if (!checkPermission(permission.edit)) return
  if (!editRow.value.type) {
    editRow.value = row
    row.isEdit = true
  }
}

// 保存单行
const save = debounce(
  async function (row) {
    if (!editAll.value && !row.isEdit && editRow.value.type) {
      if (!editRow.value.code) {
        crud.notify('请填写代号', CRUD.NOTIFICATION_TYPE.WARNING)
        return
      }
      try {
        if (editRow.value.code !== editRow.value.sourceCode || editRow.value.number !== editRow.value.sourceNumber) {
          await crudApi.save([{ ...editRow.value }])
          crud.notify('保存成功', CRUD.NOTIFICATION_TYPE.SUCCESS)
          editRow.value.sourceCode = editRow.value.code
          editRow.value.sourceNumber = editRow.value.number
        }
      } catch (error) {
        editRow.value.code = editRow.value.sourceCode
        editRow.value.number = editRow.value.sourceNumber
        console.log('保存编号配置', error)
      } finally {
        editRow.value.isEdit = false
        editRow.value = {}
      }
    }
  },
  200,
  false
)

// 获取参数
function getParams() {
  const data = []
  crud.data.forEach(v => {
    if (v.code !== v.sourceCode || v.number !== v.sourceNumber) {
      data.push({
        type: v.type,
        code: v.code,
        number: v.number
      })
    }
  })
  return data
}

// 保存全部
const saveAll = debounce(
  async function (row) {
    const params = getParams()
    if (!params.length) {
      closeEditAll()
      return
    }
    if (!crud.data.every(v => !!v.code)) {
      crud.notify('请填写代号', CRUD.NOTIFICATION_TYPE.WARNING)
      return
    }
    try {
      submitLoading.value = true
      await crudApi.save(params)
      crud.notify('保存成功', CRUD.NOTIFICATION_TYPE.SUCCESS)
      editAll.value = false
      crud.refresh()
    } catch (error) {
      console.log('保存编号配置', error)
    } finally {
      submitLoading.value = false
    }
  },
  200,
  false
)

// 编辑
function openEditAll() {
  editAll.value = true
  crud.data.forEach(v => {
    v.isEdit = true
  })
}

// 取消编辑
function closeEditAll() {
  editAll.value = false
  editRow.value = {}
  crud.data.forEach(v => {
    v.code = v.sourceCode
    v.number = v.sourceNumber
    v.isEdit = false
  })
}
</script>

<style lang="scss" scoped>
$default-cell-mask-color: #52f09840;
::v-deep(.mask-td) {
  .cell {
    &:after {
      background-color: $default-cell-mask-color;
    }
  }
}
::v-deep(.el-table) {
  th,
  td {
    padding: 0;
  }
  .cell {
    line-height: 36px;
    padding:0;
  }
  th .cell {
    padding: 0 10px;
  }
  td:first-child .cell {
    padding: 0;
  }
  .el-table__body .el-input__inner,
  .el-table__body .el-textarea__inner {
    border-radius: 0;
  }

  .cell {
    .el-input__inner {
      border: none;
    }
    .el-input-number__increase {
      border-left: none;
      margin-right: 10px;
    }
    .el-input-number__decrease {
      border-right: none;
      margin-left: 10px;
    }
  }
}
</style>
