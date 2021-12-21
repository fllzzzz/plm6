<template>
  <div class="app-container">
    <!--工具栏-->
    <div class="head-container">
      <mHeader />
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      default-expand-all
      class="assembly-table"
      style="width: 100%"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column type="expand">
        <template #default="props">
          <div :key="`'singleTable${props.row.id}'`" style="padding:10px 50px;">
            <common-table
              :key="`'singleTable${props.row.id}'`"
              :data="props.row.detailClassifyList"
              class="customer-table"
              row-key="rowKey"
              default-expand-all
              :tree-props="{ children: 'children', hasChildren: 'hasChildren' }"
              style="width: 100%;"
            >
              <el-table-column key="dictionaryName" prop="dictionaryName" label="报销种类" align="center">
                <template v-slot="scope">
                  <span>{{ scope.row.dictionaryName }}</span>
                </template>
              </el-table-column>
              <el-table-column key="applyAmount" prop="applyAmount" label="申请金额" align="center">
                <template v-slot="scope">
                  <span>{{ scope.row.applyAmount }}</span>
                </template>
              </el-table-column>
              <el-table-column key="invoiceType" prop="invoiceType" label="发票类型" align="center">
                <template v-slot="scope">
                  <span>{{ scope.row.invoiceType }}</span>
                </template>
              </el-table-column>
              <el-table-column key="inputTax" prop="inputTax" label="进项税额" align="center">
                <template v-slot="scope">
                  <span>{{ scope.row.inputTax }}</span>
                </template>
              </el-table-column>
              <el-table-column key="taxRate" prop="taxRate" label="税率" align="center">
                <template v-slot="scope">
                  <span>{{ scope.row.taxRate }}</span>
                </template>
              </el-table-column>
            </common-table>
          </div>
        </template>
      </el-table-column>
      <el-table-column prop="projectName"  key="projectName" :show-overflow-tooltip="true" align="center" label="项目">
        <template v-slot="scope">
          <span>{{ scope.row.projectName }}</span>
        </template>
      </el-table-column>
      <!-- <el-table-column prop="projectName" :show-overflow-tooltip="true" align="center" label="业务类型">
        <template v-slot="scope">
          <span>{{ scope.row.projectName }}</span>
        </template>
      </el-table-column> -->
      <el-table-column prop="applyDepartName"  key="applyDepartName" :show-overflow-tooltip="true" align="center" label="申请部门">
        <template v-slot="scope">
          <span>{{ scope.row.applyDepartName }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="applyUserName"  key="applyUserName" :show-overflow-tooltip="true" align="center" label="申请人">
        <template v-slot="scope">
          <span>{{ scope.row.applyUserName }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="applyDate"   key="applyDate" :show-overflow-tooltip="true" align="center" label="申请日期">
        <template v-slot="scope">
          <span v-empty-text v-parse-time="'{y}-{m}-{d}'">{{ scope.row.applyDate }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="applyAmount"  key="applyAmount" :show-overflow-tooltip="true" align="center" label="申请金额">
        <template v-slot="scope">
          <span>{{ scope.row.applyAmount ? scope.row.applyAmount.toThousand() : '' }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="actuallyPayAmount"  key="actuallyPayAmount" :show-overflow-tooltip="true" align="center" label="实付金额">
        <template v-slot="scope">
          <span>{{ scope.row.actuallyPayAmount ? scope.row.actuallyPayAmount.toThousand() : '' }}</span>
        </template>
      </el-table-column>
      <!-- <el-table-column prop="actuallyPayAmount" :show-overflow-tooltip="true" align="center" label="进项税额">
        <template v-slot="scope">
          <span>{{ scope.row.actuallyPayAmount? scope.row.actuallyPayAmount.toThousand(): '' }}</span>
        </template>
      </el-table-column> -->
      <el-table-column prop="paymentUnit"  key="paymentUnit" :show-overflow-tooltip="true" align="center" label="付款单位">
        <template v-slot="scope">
          <span>{{ scope.row.paymentUnit }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="collectionUserName"  key="collectionUserName" :show-overflow-tooltip="true" align="center" label="收款人">
        <template v-slot="scope">
          <span>{{ scope.row.collectionUserName }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="writtenByName"  key="writtenByName" :show-overflow-tooltip="true" align="center" label="填报人">
        <template v-slot="scope">
          <span>{{ scope.row.writtenByName }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="createTime"  key="createTime" :show-overflow-tooltip="true" align="center" label="填报日期">
        <template v-slot="scope">
          <span v-empty-text v-parse-time="'{y}-{m}-{d}'">{{ scope.row.createTime }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="confirmUserName"  key="confirmUserName" :show-overflow-tooltip="true" align="center" label="确认人">
        <template v-slot="scope">
          <span>{{ scope.row.confirmUserName }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="confirmTime"  key="confirmTime" :show-overflow-tooltip="true" align="center" label="确认日期">
        <template v-slot="scope">
          <span v-parse-time="'{y}-{m}-{d}'" v-if="scope.row.confirmTime">{{ scope.row.confirmTime }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('confirmStatus')" key="confirmStatus" prop="confirmStatus" label="状态" align="center" width="110px">
        <template v-slot="scope">
          <div>{{ scope.row.confirmStatus? reimbursementTypeEnum.VL[scope.row.confirmStatus]: ''}}</div>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('source')" key="source" prop="source" label="来源" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.source? systemTypeEnum.VL[scope.row.source]: '' }}</div>
      </template>
    </el-table-column>
      <el-table-column
        v-if="checkPermission([...permission.edit])"
        label="操作"
        width="260px"
        align="center"
      >
        <template v-slot="scope">
          <common-button icon="el-icon-view" type="primary" size="mini" @click="openDetail(scope.row, 'detail')"/>
          <common-button icon="el-icon-s-check" type="primary" size="mini" v-permission="permission.audit" @click="openDetail(scope.row, 'audit')" v-if="scope.row.confirmStatus==reimbursementTypeEnum.ENUM.AUDITING.V"/>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <mForm />
    <mDetail :collectionInfo="currentInfo" :type="showType" v-model="detailVisble" @success="crud.toQuery" />
  </div>
</template>

<script setup>
import crudApi from '@/api/contract/supplier-manage/reimbursement'
import { ref, watch } from 'vue'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import pagination from '@crud/Pagination'
import { mapGetters } from '@/store/lib'
import mHeader from './module/header'
import mForm from './module/form'
import mDetail from './module/detail'
import { reimbursementTypeEnum, systemTypeEnum, invoiceTypeEnum } from '@enum-ms/contract'
import { DP } from '@/settings/config'
import useDict from '@compos/store/use-dict'
import { paymentFineModeEnum } from '@enum-ms/finance'
import { toThousand } from '@/utils/data-type/number'

// crud交由presenter持有
const permission = {
  get: ['reimbursement:get'],
  add: ['reimbursement:add'],
  edit: ['reimbursement:edit'],
  audit: ['reimbursement:audit'],
}

const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false,
}

const tableRef = ref()
const currentInfo = ref({})
const showType = ref('detail')
const detailVisble = ref(false)
const dict = useDict(['payment_reason'])
const { crud, columns, CRUD } = useCRUD(
  {
    title: '报销填报',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    invisibleColumns: ['tax', 'invoiceDate', 'invoiceNo', 'createTime', 'auditTime'],
    hasPagination: true,
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.collection',
  paginate: true,
  extraHeight: 157,
})

function openDetail(row, type) {
  currentInfo.value = row
  showType.value = type
  detailVisble.value = true
}

CRUD.HOOK.handleRefresh = (crud, data) => {
  data.data.content = data.data.content.map((v) => {
    v.detailList = []
    if(v.detailClassifyList && v.detailClassifyList.length>0){
      v.detailClassifyList.map(k=>{
        k.rowKey = k.expenseTypeId
        k.dictionaryName = k.expenseTypeName
        if (k.reimbursementDetailList && k.reimbursementDetailList.length > 0) {
          k.children = k.reimbursementDetailList
          k.applyAmount = 0
          k.invoiceAmount = 0
          k.inputTax = 0
          k.children.map((val) => {
            v.detailList.push({
              applyAmount: val.applyAmount,
              dictionaryId: val.dictionaryId,
              expenseTypeId: val.expenseTypeId,
              id: val.id,
              inputTax: val.inputTax,
              invoiceAmount: val.invoiceAmount,
              invoiceNo: val.invoiceNo,
              invoiceType: val.invoiceType,
              taxRate: val.taxRate
            })
            val.rowKey = `${k.expenseTypeId}__${val.id}`
            k.applyAmount += val.applyAmount
            k.invoiceAmount += val.invoiceAmount
            k.inputTax += val.inputTax
          })
        }else{
          k.children = []
          k.applyAmount = undefined
          k.invoiceAmount = undefined
          k.inputTax = undefined
        }
      })
    }
    return v
  })
}
</script>

<style lang="scss" scoped>
// .customer-table {
//   ::v-deep(th) {
//     border: none;
//   }
//   ::v-deep(td) {
//     border: none;
//   }
//   ::v-deep(th.is-leaf) {
//     border: none;
//   }
//   ::v-deep(.el-input__inner) {
//     padding: 0;
//     padding-left: 5px;
//     text-align: left;
//   }
//   &::before {
//     width: 0;
//   }
// }
// .assembly-table {
//   ::v-deep(.cell) {
//     padding-left: 0;
//     padding-right: 0;
//   }
//   ::v-deep(thead.is-group th) {
//     background: #fff;
//   }
// }
</style>