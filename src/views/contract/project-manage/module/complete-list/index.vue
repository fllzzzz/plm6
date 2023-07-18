<template>
  <div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      return-source-data
      :showEmptySymbol="false"
      style="width: 100%"
    >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" fixed="left"/>
      <el-table-column
        v-if="columns.visible('serialNumber')"
        key="serialNumber"
        prop="serialNumber"
        :show-overflow-tooltip="false"
        min-width="130"
        align="center"
        label="合同编号"
        fixed="left"
      />
      <el-table-column v-if="columns.visible('shortName')" key="shortName" prop="shortName" :show-overflow-tooltip="true" label="项目" min-width="150">
        <template v-slot="scope">
          <el-tooltip :content="scope.row.serialNumber+' '+scope.row.name" :show-after="50" placement="top">
            <span>{{scope.row.shortName}}</span>
          </el-tooltip>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('businessType')"
        key="businessType"
        prop="businessType"
        label="业务类型"
        align="center"
        width="90"
      >
        <template v-slot="scope">
          <div>{{ scope.row.businessType ? businessTypeEnum.VL[scope.row.businessType] : '-' }}</div>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('projectType')"
        key="projectType"
        prop="projectType"
        label="项目类型"
        align="center"
        width="100"
      >
        <template v-slot="scope">
          <div>{{ scope.row.projectType ? projectTypeEnum.VL[scope.row.projectType] : '-' }}</div>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('projectContent')"
        key="projectContent"
        :show-overflow-tooltip="true"
        prop="projectContent"
        label="项目内容"
        align="center"
        min-width="150"
      >
        <template v-slot="scope">
          <div>{{ scope.row.projectContent }}</div>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('contractAmount')"
        key="contractAmount"
        prop="contractAmount"
        :show-overflow-tooltip="true"
        min-width="120"
        align="center"
        :label="`合同金额(元)`"
      >
        <template v-slot="scope">
          <span>{{ scope.row.contractAmount? toThousand(scope.row.contractAmount,decimalPrecision.contract): '-' }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('allDays')" key="allDays" prop="allDays" label="总工期(天)" align="center" width="80">
        <template v-slot="scope">
          <div>{{ scope.row.allDays }}</div>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('alreadyDays')"
        key="alreadyDays"
        prop="alreadyDays"
        label="已用时(天)"
        align="center"
        width="80"
      >
        <template v-slot="scope">
          <div>{{ scope.row.alreadyDays }}</div>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('signerName')" key="signerName" prop="signerName" label="签约人" align="center" width="100px">
        <template v-slot="scope">
          <div>{{ scope.row.signerName }}</div>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column label="操作" width="180px" align="center" fixed="right">
        <template v-slot="scope">
          <common-button
            size="mini"
            icon="el-icon-view"
            type="primary"
            @click="openContractInfo(scope.row)"
            v-if="checkPermission(projectListPM.detail)"
          />
          <el-popconfirm title="确认完工么?" @confirm="changeStatus(scope.row, projectStatusEnum.COMPLETE.V)" v-if="checkPermission(permission.completeConfirm)">
            <template #reference>
              <common-button
              size="mini"
              >完工</common-button>
            </template>
          </el-popconfirm>
        </template>
      </el-table-column>
    </common-table>
    <contract-info
      :project-id="contractInfoRowId"
      :project-status="projectStatus"
      :project-name="projectName"
      v-model="contractInfoVisible"
      :btnShow="false"
    />
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import crudApi, { editStatus } from '@/api/contract/project'
import { ref, defineEmits } from 'vue'

import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import { projectTypeEnum, businessTypeEnum, projectStatusEnum } from '@enum-ms/contract'
import { toThousand } from '@data-type/number'
import { projectListPM } from '@/page-permission/contract'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

import pagination from '@crud/Pagination'
import contractInfo from '@/views/contract/info/index'

const permission = projectListPM.completeList

const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false
}

const { decimalPrecision } = useDecimalPrecision()

const tableRef = ref()
const contractInfoRowId = ref()
const projectStatus = ref()
const projectName = ref()
const contractInfoVisible = ref(false)
const emit = defineEmits(['success'])

const { crud, columns, CRUD } = useCRUD(
  {
    title: '完工列表',
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.completeProject',
  paginate: true,
  extraHeight: 40
})

async function changeStatus(data, val) {
  try {
    await editStatus(data.id, val, true)
    crud.notify(`“${data.name}” 确认完工成功`, CRUD.NOTIFICATION_TYPE.SUCCESS)
    crud.refresh()
    setTimeout(() => {
      emit('success')
    }, 400)
  } catch (error) {
    console.log(error)
  }
}

function openContractInfo(row) {
  contractInfoRowId.value = row.id
  projectStatus.value = row.status
  projectName.value = row.name
  contractInfoVisible.value = true
}
CRUD.HOOK.beforeRefresh = () => {
  crud.query.outFinishStatus = 1
}
</script>

<style lang="scss" scoped>
::v-deep(.abnormal-row) {
  background: #e8f4ff;
}
::v-deep(.hidden-select) {
  td:nth-child(1) {
    .cell {
      opacity: 0;
    }
  }
}
$font-size: 1.5em;
.child {
  width: $font-size;
  height: $font-size;
  display: inline-block;
  border: 1px solid;
  border-radius: 50%;
  line-height: $font-size;
}
</style>
