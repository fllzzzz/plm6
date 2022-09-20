<template>
  <el-card v-if="checkPermission(permission.get)" class="box-card" shadow>
    <template #header class="clearfix">
      <span class="title">配套件安装设置</span>
      <div v-if="checkPermission(permission.edit) && changed" style="float: right">
        <common-button size="small" type="warning" @click="cancel">取消修改</common-button>
        <common-button :loading="saveLoading" size="small" type="success" @click="submit">保存</common-button>
      </div>
    </template>
    <div v-loading="loading || saveLoading">
      <common-table
        ref="tableRef"
        v-loading="crud.loading"
        :data="crud.data"
        :empty-text="crud.emptyText"
        :max-height="maxHeight"
        :border="false"
        style="width: 100%;"
        :stripe="false"
        :showEmptySymbol="false"
      >
        <el-table-column key="classifyName" prop="classifyName" label="配套件产品分类" align="center" />
        <el-table-column key="boolNoReport" prop="boolNoReport" label="即入即安" align="center" width="180">
          <template v-slot="scope">
            <common-radio v-model="scope.row.boolNoReport" :options="whetherEnum.ENUM" type="enum" @change="dataChange(scope.row)" :disabled="!checkPermission(permission.edit)"/>
          </template>
        </el-table-column>
      </common-table>
    </div>
  </el-card>
</template>

<script setup>
import crudApi, { editAuxiliaryReportMethod } from '@/api/project-manage/install-config'
import { ref, computed, watch } from 'vue'

import checkPermission from '@/utils/system/check-permission'
import { whetherEnum } from '@enum-ms/common'
import { mapGetters } from '@/store/lib'
import { installConfigPM } from '@/page-permission/project'
import { ElMessage } from 'element-plus'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'

const permission = installConfigPM.auxiliaryMaterial

const { globalProjectId } = mapGetters(['globalProjectId'])

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const loading = ref(false)
const saveLoading = ref(false)
const sourceData = ref([])
const listChange = ref([])

const tableRef = ref()

const { crud, CRUD } = useCRUD(
  {
    title: '配套件安装设置',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    requiredQuery: ['projectId'],
    hasPagination: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  paginate: true,
  extraHeight: 130
})

watch(
  () => globalProjectId.value,
  (val) => {
    if (val) {
      crud.query.projectId = globalProjectId.value
      crud.toQuery()
    }
  },
  { immediate: true, deep: true }
)

function dataChange(row) {
  if (row.boolNoReport !== row.sourceRow.boolNoReport) {
    if (listChange.value.findIndex(v => v.classifyId === row.classifyId) < 0) {
      listChange.value.push({
        projectId: row.projectId,
        classifyId: row.classifyId,
        boolNoReport: row.boolNoReport
      })
    }
  } else {
    if (listChange.value.findIndex(v => v.classifyId === row.classifyId) > -1) {
      listChange.value.splice(listChange.value.findIndex(v => v.classifyId === row.classifyId), 1)
    }
  }
}

const changed = computed(() => {
  return !!listChange.value.length
})

CRUD.HOOK.handleRefresh = (crud, data) => {
  sourceData.value = JSON.parse(JSON.stringify(data.data.content))
}

async function submit() {
  saveLoading.value = true
  try {
    await editAuxiliaryReportMethod(listChange.value)
    ElMessage.success('修改成功')
    listChange.value = []
    crud.refresh()
  } catch (error) {
    console.log('安装设置', error)
  } finally {
    saveLoading.value = false
  }
}

function cancel() {
  listChange.value = []
  crud.refresh()
}

</script>

<style lang="scss" scoped>
.title{
    height: 32px;
    line-height: 32px;
}
</style>
