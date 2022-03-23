<template>
  <div>
    <div v-show="!lineName">
      <div class="my-code">点击字典查看详情</div>
    </div>
    <div v-show="lineName">
      <!--工具栏-->
      <div class="head-container">
        <mHeader ref="header" :permission="permission" />
      </div>
      <!--表格渲染-->
      <common-table
        ref="tableRef"
        v-loading="crud.loading"
        :data="crud.data"
        :empty-text="crud.emptyText"
        :max-height="maxHeight"
        style="width: 100%"
        return-source-data
        :showEmptySymbol="false"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column
          v-if="columns.visible('name')"
          key="name"
          prop="name"
          :show-overflow-tooltip="true"
          label="所属字典"
          width="120px"
        />
        <el-table-column
          v-if="columns.visible('remark')"
          key="remark"
          prop="remark"
          :show-overflow-tooltip="true"
          label="字典标签"
          width="120px"
        />
        <el-table-column
          v-if="columns.visible('label')"
          key="label"
          prop="label"
          :show-overflow-tooltip="true"
          label="字典值"
          width="120px"
        />
        <el-table-column
          v-if="columns.visible('sort')"
          key="sort"
          prop="sort"
          :show-overflow-tooltip="true"
          label="排序"
          min-width="160px"
        />
        <!--编辑与删除-->
        <el-table-column
          v-if="checkPermission([...permission.edit, ...permission.del])"
          label="操作"
          width="130px"
          align="center"
          fixed="right"
        >
          <template v-slot="scope">
            <udOperation :data="scope.row" :permission="permission" />
          </template>
        </el-table-column>
      </common-table>
      <!--分页组件-->
      <pagination />
      <mForm />
    </div>
  </div>
</template>

<script setup>
import crudApi from '@/api/system/dict-detail'
import { defineExpose, ref, defineProps, watch, computed } from 'vue'
import checkPermission from '@/utils/system/check-permission'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mForm from './module/form'

// crud交由presenter持有
const permission = {
  get: ['systemDictDetail:get'],
  add: ['systemDictDetail:add'],
  edit: ['systemDictDetail:edit'],
  del: ['systemDictDetail:del']
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '字典详情',
    sort: [],
    permission: { ...permission },
    crudApi: { ...crudApi },
    queryOnPresenterCreated: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.dict-detail-card',
  paginate: true,
  extraHeight: 157
})

const props = defineProps({
  line: {
    type: Object,
    default: () => {}
  }
})

const lineName = computed(() => {
  return props.line && props.line.name
})

watch(
  () => lineName,
  (val) => {
    if (val.value) {
      crud.toQuery()
    }
  },
  { deep: true, immediate: true }
)

CRUD.HOOK.beforeRefresh = () => {
  crud.query.name = props.line.name
  return !!crud.query.name
}

CRUD.HOOK.beforeSubmit = () => {
  crud.form.name = props.line.name
}

defineExpose({
  permission,
  toAdd: crud.toAdd
})
</script>
