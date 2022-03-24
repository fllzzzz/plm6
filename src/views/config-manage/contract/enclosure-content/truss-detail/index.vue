<template>
  <div>
    <div v-show="!lineCode">
      <div class="my-code">点击围护配置查看详情</div>
    </div>
    <div v-show="lineCode">
      <!--工具栏-->
      <!-- <div class="head-container">
        <mHeader />
      </div> -->
      <!--表格渲染-->
      <common-button class="filter-item" size="mini" type="primary" @click.stop="isEdit=true" v-if="isEdit===false" :disabled="handleDisabled" v-permission="permission.edit">修改</common-button>
      <template v-else>
        <common-button class="filter-item" size="mini" @click.stop="onCancel" :disabled="handleDisabled">取消</common-button>
        <common-button class="filter-item" size="mini" type="primary" @click.stop="onSubmit" :disabled="handleDisabled">提交</common-button>
      </template>
      <common-table
        ref="tableRef"
        v-loading="crud.loading"
        :data="crud.data"
        :empty-text="crud.emptyText"
        :max-height="maxHeight"
        :cell-class-name="wrongCellMask"
        return-source-data
        :showEmptySymbol="false"
        style="width: 100%;margin-top:10px;"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column
          v-if="columns.visible('remark')"
          key="remark"
          prop="remark"
          :show-overflow-tooltip="true"
          label="名称"
        >
          <template v-slot="scope">
            {{ scope.row.remark }}
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('label')"
          key="label"
          prop="label"
          label="值"
        >
          <template v-slot="scope">
            <template v-if="isEdit===true">
              <el-input-number
                class="align-left"
                v-model="scope.row.label"
                placeholder="请填写"
                type="text"
                controls-position="right"
                style="width: 200px"
                :min="0"
              />
            </template>
            <template v-else>
              {{ scope.row.label }}
            </template>
          </template>
        </el-table-column>
      </common-table>
    </div>
  </div>
</template>

<script setup>
import crudApi, { edit } from '@/api/contract/enclosure-config/truss-detail'
import { defineExpose, ref, defineProps, watch, computed } from 'vue'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import useTableValidate from '@compos/form/use-table-validate'
import { enclosureInfoConfigPM } from '@/page-permission/config'

// crud交由presenter持有
const permission = enclosureInfoConfigPM.detailInfo

const tableRef = ref()
const isEdit = ref(false)
const handleDisabled = ref(false)
const { crud, columns, CRUD } = useCRUD(
  {
    title: '桁架楼层板',
    sort: [],
    permission: { ...permission },
    crudApi: { ...crudApi },
    queryOnPresenterCreated: false,
    hasPagination: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.truss-detail-card',
  extraHeight: 40
})

const props = defineProps({
  line: {
    type: Object,
    default: () => {}
  }
})

const lineCode = computed(() => {
  return props.line && props.line.code
})

const tableRules = {
  label: [{ required: true, max: 20, message: '不能超过50个字符', trigger: 'blur', type: 'number' }]
}
const { tableValidate, wrongCellMask } = useTableValidate({ rules: tableRules })

watch(
  () => lineCode,
  (val) => {
    if (val.value) {
      crud.toQuery()
    }
  },
  { deep: true, immediate: true }
)

CRUD.HOOK.beforeRefresh = () => {
  crud.query.code = props.line.code
  crud.query.type = 3
  return !!crud.query.code
}

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.label = v.label ? Number(v.label) : undefined
    return v
  })
}

function onCancel() {
  isEdit.value = false
  handleDisabled.value = false
  crud.refresh()
}

async function onSubmit() {
  const { validResult, dealList } = tableValidate(crud.data)
  if (validResult) {
    crud.data = dealList
  } else {
    return validResult
  }
  handleDisabled.value = true
  try {
    const submitData = {
      code: lineCode.value,
      dictionaryDetails: crud.data
    }
    await edit(submitData)
    onCancel()
    crud.notify('操作成功', CRUD.NOTIFICATION_TYPE.SUCCESS)
  } catch (error) {
    console.log('修改桁架楼层板', error)
  }
}

defineExpose({
  permission,
  toAdd: crud.toAdd
})
</script>
